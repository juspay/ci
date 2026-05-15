{-# LANGUAGE OverloadedStrings #-}

{- | How a 'CI.Node.NodeId' actually executes: locally against the
pipeline's pinned worktree, or remotely against a snapshotted
@git bundle@ uploaded over an SSH-shaped runner. The split lives
here so the only module that knows about runner + bundle command
shapes is this one — every other consumer ('CI.Pipeline',
'CI.Verdict', etc.) sees only the typed value plus the 'commandFor'
rendering.

Remote execution: the runner ships the target-platform @just@
*derivation* (via @nix-store --export | nix-store --import@) and
the remote @nix-store --realise@s it to a native binary. That
sidesteps the cross-arch problem entirely — we don't ship our
locally-built binary; the remote's substituter chain fetches the
right one for its own arch. The remote needs @nix@, @git@, and
whatever the recipes themselves use, but @just@ specifically does
*not* need to be pre-installed on PATH.

The runner string is selected from the 'Host': a bare @hostname@
(or @user\@hostname@) uses @ssh -T@; a literal @pu connect \<name\>@
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
import CI.Justfile (RecipeName, recipeCommand)
import CI.Nix (justDrvFor)
import CI.Platform (Platform)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)

{- | Where a node runs. 'Local' means the spawning process-compose
already @chdir@'d into the pinned worktree (see
'CI.Pipeline.RunMode') so the @just@ subprocess inherits the
right cwd; @Ssh host sha targetPlat@ means the command is a
wrapper that bundles HEAD to @host@, ships the @just@ derivation
for @targetPlat@, clones the repo, checks out @sha@, and runs the
realised @just@ in that remote checkout.

The 'Sha' rides inside 'Ssh' (not as a separate parameter) because
it's only meaningful on the SSH path. The 'Platform' carries the
remote's target arch so we know which drv to ship — same for ssh
and pu targets; the dispatch is at @remoteRunner@'s prefix check.

Despite the name, @Ssh@ also covers incus-style runners spelled as
@pu connect \<name\>@ in @hosts.json@ — the constructor names the
*kind* of execution (remote, SSH-shaped command-prefix), not the
literal binary.
-}
data Transport = Local | Ssh Host Sha Platform

{- | The shell command process-compose runs for one node. Local
nodes use the same @just --no-deps@ invocation as before
(preserved verbatim from 'CI.Justfile.recipeCommand'); remote
nodes emit the drv-copy + bundle + realise + run dance.
-}
commandFor :: Transport -> RecipeName -> Text
commandFor Local r = recipeCommand r
commandFor (Ssh host sha targetPlat) r = remoteCommand host sha targetPlat r

{- | The shell-tokens prefix that runs a command on this 'Host'.
Two flavours:

  * @ssh -T \<host\>@ — the default. @-T@ suppresses TTY allocation
    so binary stdin (the @git bundle@ stream) survives unmolested.

  * @pu connect \<name\>@ — used verbatim when the host string
    starts with that literal prefix. The @pu@ incus client accepts
    the same "command-prefix + quoted argv" shape as @ssh@, so a
    host configured as @"pu connect builder-mac"@ in
    @~\/.config\/ci\/hosts.json@ Just Works as a drop-in.

Detection is by exact prefix because the alternative ("anything
with spaces in it") is too permissive — @user\@host@ forms with
embedded options like @"-p 2222 builder"@ are plausible future SSH
variants and shouldn't be misclassified.
-}
remoteRunner :: Host -> Text
remoteRunner host
    | "pu connect " `T.isPrefixOf` h = h
    | otherwise = "ssh -T " <> h
  where
    h = display host

{- | The full remote wrapper for one recipe execution. The shape:

  1. @nix-store --export $(...just-drv-closure...) | <runner>
     nix-store --import@ — push the @just@ *derivation* for the
     remote's target platform (a tiny file of recipe metadata, not
     the binary). Idempotent on the remote.
  2. @T=$(<runner> mktemp -d)@ — captures a fresh remote tempdir.
  3. @git bundle create -@ piped through @<runner>@ to materialise
     the bundle on the remote, clone it, and check out the pinned
     SHA.
  4. @<runner> "cd ... && \$(nix-store --realise <drv>)/bin/just
     --no-deps <recipe>"@ — realise the drv on the remote (the
     remote's substituter chain — typically @cache.nixos.org@ —
     fetches the native binary for its own arch), then run it
     against the cloned repo. The realised output path is computed
     on the *remote* side (note the escaped @\\$()@ so the
     subshell evaluates there, not locally).
  5. @<runner> "rm -rf $T"@ — cleanup, always runs.
  6. @exit $rc@ — propagate the build's exit code as the node's.

@<runner>@ is 'remoteRunner': @ssh -T <host>@ for ordinary SSH
targets, @pu connect <name>@ for incus addresses. The drv-copy
pipe is transport-agnostic and works the same way over both runners.

The remote needs Nix installed (which it must, to run the recipes
themselves) — @just@ specifically does *not* need to be on PATH.
-}
remoteCommand :: Host -> Sha -> Platform -> RecipeName -> Text
remoteCommand host sha targetPlat recipe =
    T.intercalate " && " (drvCopyStep : setupSteps)
        <> "; "
        <> r
        <> " \"cd $T/src && \\$(nix-store --realise "
        <> T.pack drv
        <> ")/bin/just --no-deps "
        <> display recipe
        <> "\"; rc=$?; "
        <> r
        <> " \"rm -rf $T\"; exit $rc"
  where
    r = remoteRunner host
    drv = justDrvFor targetPlat
    drvCopyStep =
        "nix-store --export $(nix-store --query --requisites "
            <> T.pack drv
            <> ") | "
            <> r
            <> " nix-store --import > /dev/null"
    setupSteps =
        [ "T=$(" <> r <> " mktemp -d)"
        , "git bundle create - --all 2>/dev/null | "
            <> r
            <> " \"cat > $T/repo.bundle && cd $T && git clone --quiet repo.bundle src && cd src && git -c advice.detachedHead=false checkout --quiet "
            <> display sha
            <> "\""
        ]
