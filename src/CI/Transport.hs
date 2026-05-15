{-# LANGUAGE OverloadedStrings #-}

{- | How a 'CI.Node.NodeId' actually executes: locally against the
pipeline's pinned worktree, or remotely against a snapshotted
@git bundle@ uploaded over a SSH-shaped runner. The split lives
here so the only module that knows about runner + bundle command
shapes is this one — every other consumer ('CI.Pipeline',
'CI.Verdict', etc.) sees only the typed value plus the 'commandFor'
rendering.

The remote command shape mirrors kolu's reference: three runner
invocations per node — @mktemp@ to provision a remote tempdir, a
bundle-piped clone+checkout, and the @just --no-deps@ invocation —
with a final cleanup that respects the exit code of the build
itself. Three round trips is wasteful but simple; an upload-once
setup phase is a future optimisation.

The runner is selected from the 'Host' string: a bare @hostname@ (or
@user\@hostname@) uses @ssh -T@; a literal @pu connect \<name\>@
prefix uses the @pu@ incus client verbatim instead. See
'remoteRunner'.
-}
module CI.Transport (
    Transport (..),
    commandFor,
    remoteRunner,
)
where

import CI.Git (Sha)
import CI.Hosts (Host)
import CI.Justfile (RecipeName, justBin, recipeCommand)
import CI.Platform (Platform)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)

{- | Where a node runs. 'Local' means the spawning process-compose
already @chdir@'d into the pinned worktree (see
'CI.Pipeline.RunMode') so the @just@ subprocess inherits the
right cwd; @Ssh host sha localPlat targetPlat@ means the command
is a wrapper that bundles HEAD to @host@, clones it there, checks
out @sha@, and runs @just@ in that remote checkout.

The 'Sha' rides inside 'Ssh' (not as a separate parameter) because
it's only meaningful on the SSH path — a local node runs against
the worktree pc has already @chdir@'d into, so the @HEAD@ SHA is
redundant. Keeping the dependency local to the constructor it
needs avoids a parallel @Maybe Sha@ threaded through every caller.

Despite the name, @Ssh@ also covers incus-style runners spelled as
@pu connect \<name\>@ in @hosts.json@ — the constructor names the
*kind* of execution (remote, SSH-shaped command-prefix), not the
literal binary. See 'remoteRunner'.

The two 'Platform's (runner-local and target) let this module
derive the arch-compatibility decision internally: callers hand
over both platforms and Transport classifies them via
'archKindFor'. That keeps arch semantics — "can we nix-copy our
@just@ closure to this remote?" — encapsulated here rather than
spread across the caller and the @sshCommand@ shape.
-}
data Transport = Local | Ssh Host Sha Platform Platform

{- | Whether the remote can execute binaries from the runner's own
@\/nix\/store@.

  * 'NativeArch' — same platform (and assumed same architecture).
    The wrapper @nix-store --export@s the local @just@ closure and
    imports it on the remote, then invokes @just@ via the absolute
    @\/nix\/store@ path. Works even when @just@ isn't on the
    remote's PATH.

  * 'ForeignArch' — different platform (e.g. linux runner reaching
    a darwin remote). Our local binary won't run there, so we skip
    the closure-copy and invoke @just@ from the remote's PATH.

Module-private: callers hand 'commandFor' the two 'Platform's
('Ssh''s third and fourth fields) and 'archKindFor' produces the
'ArchKind' here. The classifier lives next to the consumer so a
refinement of arch detection (e.g. distinguishing x86_64-linux
from aarch64-linux within one 'Platform') touches this module
only.
-}
data ArchKind = NativeArch | ForeignArch

{- | Same-platform target ⇒ same-arch (modulo cross-arch within one
'Platform', e.g. x86_64-linux runner reaching aarch64-linux; we
don't refine here yet — the cost is only the closure-copy step,
which fails fast with a clear "Exec format error" if it bites).
-}
archKindFor :: Platform -> Platform -> ArchKind
archKindFor localPlat targetPlat
    | localPlat == targetPlat = NativeArch
    | otherwise = ForeignArch

{- | The shell command process-compose runs for one node. Local
nodes use the same @just --no-deps@ invocation as before
(preserved verbatim from 'CI.Justfile.recipeCommand'); remote
nodes emit the three-call bundle+clone+run dance against
'remoteRunner'.
-}
commandFor :: Transport -> RecipeName -> Text
commandFor Local r = recipeCommand r
commandFor (Ssh host sha localPlat targetPlat) r =
    sshCommand host sha r (archKindFor localPlat targetPlat)

{- | The shell-tokens prefix that runs a command on this 'Host'.
Two flavours:

  * @ssh -T \<host\>@ — the default. @-T@ suppresses TTY allocation
    so binary stdin (the @git bundle@ stream) survives unmolested.

  * @pu connect \<name\>@ — used verbatim when the host string
    starts with that literal prefix. The @pu@ incus client accepts
    the same "command-prefix + quoted argv" shape as @ssh@, so a
    host configured as @"pu connect builder-mac"@ in
    @~\/.config\/ci\/hosts.json@ Just Works as a drop-in.

The detection is by exact prefix because the alternative
("anything with spaces in it") is too permissive — @user\@host@
forms with embedded options like @"-p 2222 builder"@ are
plausible future SSH variants and shouldn't be misclassified.
-}
remoteRunner :: Host -> Text
remoteRunner host
    | "pu connect " `T.isPrefixOf` h = h
    | otherwise = "ssh -T " <> h
  where
    h = display host

{- | The full remote wrapper for one recipe execution. The shape:

  1. @nix-store --export $(...just-closure...) | <runner> nix-store
     --import@ — push our local @just@ derivation (the absolute
     @\/nix\/store\/...just@ baked in at compile time) into the
     remote's Nix store. Idempotent: if the closure is already
     there, @--import@ is a no-op. Pushing every run is simpler
     than tracking whether we've done it.
  2. @T=$(<runner> mktemp -d)@ — captures a fresh remote tempdir.
  3. @git bundle create -@ piped through @<runner>@ to materialise
     the bundle on the remote, clone it, and check out the pinned
     SHA.
  4. @<runner> "cd ... && <absolute-just> --no-deps <recipe>"@ —
     the actual build, invoked through the same absolute path the
     local runner uses (so PATH on the remote is irrelevant). Its
     exit code is captured.
  5. @<runner> "rm -rf $T"@ — cleanup, always runs.
  6. @exit $rc@ — propagate the build's exit code as the node's.

@<runner>@ is 'remoteRunner': @ssh -T <host>@ for ordinary SSH
targets, @pu connect <name>@ for incus addresses. Step 1 assumes
the remote has Nix installed (which it must, to run the recipes
themselves); the closure-export pipe is transport-agnostic and
works the same way over both runners.
-}
sshCommand :: Host -> Sha -> RecipeName -> ArchKind -> Text
sshCommand host sha recipe arch =
    T.intercalate " && " (nixCopySteps <> setupSteps)
        <> "; "
        <> r
        <> " \"cd $T/src && "
        <> remoteJust
        <> "\"; rc=$?; "
        <> r
        <> " \"rm -rf $T\"; exit $rc"
  where
    r = remoteRunner host
    setupSteps =
        [ "T=$(" <> r <> " mktemp -d)"
        , "git bundle create - --all 2>/dev/null | "
            <> r
            <> " \"cat > $T/repo.bundle && cd $T && git clone --quiet repo.bundle src && cd src && git -c advice.detachedHead=false checkout --quiet "
            <> display sha
            <> "\""
        ]
    -- Same-arch only: push the local @just@ closure so the remote
    -- can invoke it by absolute path even without @just@ on PATH.
    -- Skipped for foreign-arch (the binary wouldn't be executable
    -- there anyway).
    nixCopySteps = case arch of
        NativeArch ->
            [ "nix-store --export $(nix-store --query --requisites "
                <> T.pack justBin
                <> ") | "
                <> r
                <> " nix-store --import > /dev/null"
            ]
        ForeignArch -> []
    remoteJust = case arch of
        NativeArch -> recipeCommand recipe
        ForeignArch -> "just --no-deps " <> display recipe
