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

Every remote node runs over plain @ssh -T \<host\>@ — anything the
local @ssh@ config knows how to dial works, including aliases from
@~\/.ssh\/config@. (Incus instances are reached via a host alias
that names them; no special-case client involved at this layer.)
-}
module CI.Transport (
    Transport (Local),
    sshTransport,
    commandFor,
    remoteRunner,
)
where

import CI.Git (Sha)
import CI.Hosts (Host)
import CI.Justfile (RecipeName, recipeCommand)
import CI.Nix (realisedJust, shipJustDrv)
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

{- | Smart constructor for the remote case. The 'Ssh' data constructor
is unexported, so this is the only way to build a remote
'Transport' value — callers can't reach in and pattern-match on the
field order, and a future shape change (e.g. dropping the 'Platform'
once drv-resolution moves elsewhere) edits one signature instead of
every construction site.
-}
sshTransport :: Host -> Sha -> Platform -> Transport
sshTransport = Ssh

{- | The shell command process-compose runs for one node. Local
nodes use the same @just --no-deps@ invocation as before
(preserved verbatim from 'CI.Justfile.recipeCommand'); remote
nodes emit the drv-copy + bundle + realise + run dance.
-}
commandFor :: Transport -> RecipeName -> Text
commandFor Local r = recipeCommand r
commandFor (Ssh host sha targetPlat) r = remoteCommand host sha targetPlat r

{- | The shell-tokens prefix that runs a command on this 'Host':
@ssh -T \<host\>@. @-T@ suppresses TTY allocation so the binary
stdin (the @git bundle@ stream) survives unmolested. Anything the
local @ssh@ config knows how to dial — bare @hostname@,
@user\@host@, an alias from @~\/.ssh\/config@ — works as the host
string.
-}
remoteRunner :: Host -> Text
remoteRunner host = "ssh -T " <> display host

{- | The full remote wrapper for one recipe execution. The shape:

  1. 'CI.Nix.shipJustDrv' — push the @just@ derivation for the
     remote's target platform to its Nix store.
  2. @T=$(<runner> mktemp -d)@ — fresh remote tempdir.
  3. @git bundle create -@ piped through @<runner>@ to materialise
     the bundle on the remote, clone it, and check out the pinned
     SHA.
  4. @<runner> "cd ... && 'CI.Nix.realisedJust' "@ — realise the
     drv on the remote (its substituter chain fetches the native
     binary) and invoke @just --no-deps \<recipe\>@.
  5. @<runner> "rm -rf $T"@ — cleanup, always runs.
  6. @exit $rc@ — propagate the build's exit code as the node's.

The remote needs Nix installed (which it must, to run the recipes
themselves) — @just@ specifically does *not* need to be on PATH.
-}
remoteCommand :: Host -> Sha -> Platform -> RecipeName -> Text
remoteCommand host sha targetPlat recipe =
    T.intercalate " && " (shipJustDrv r targetPlat : setupSteps)
        <> "; "
        <> r
        <> " \"cd $T/src && "
        <> realisedJust targetPlat recipe
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
