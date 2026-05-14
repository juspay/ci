{-# LANGUAGE OverloadedStrings #-}

{- | How a 'CI.Node.NodeId' actually executes: locally against the
pipeline's pinned worktree, or remotely against a snapshotted
@git bundle@ on an SSH 'CI.Hosts.Host'. The split lives here so
the only module that knows about ssh + bundle command shapes is
this one — every other consumer ('CI.Pipeline', 'CI.Verdict', etc.)
sees only the typed value plus the 'commandFor' rendering.

The SSH command shape mirrors kolu's reference: three SSH
connections per node — @mktemp@ to provision a remote tempdir,
a bundle-piped clone+checkout, and the @just --no-deps@ invocation
— with a final cleanup that respects the exit code of the build
itself. Three round trips is wasteful but simple; an upload-once
setup phase is a future optimisation.
-}
module CI.Transport (
    Transport (..),
    commandFor,
)
where

import CI.Git (Sha)
import CI.Hosts (Host)
import CI.Justfile (RecipeName, recipeCommand)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)

{- | Where a node runs. 'Local' means the spawning process-compose
already @chdir@'d into the pinned worktree (see
'CI.Pipeline.RunMode') so the @just@ subprocess inherits the
right cwd; @Ssh host sha@ means the command is a wrapper that
bundles HEAD to @host@, clones it there, checks out @sha@, and
runs @just@ in that remote checkout.

The 'Sha' rides inside 'Ssh' (not as a separate parameter) because
it's only meaningful on the SSH path — a local node runs against
the worktree pc has already @chdir@'d into, so the @HEAD@ SHA is
redundant. Keeping the dependency local to the constructor it
needs avoids a parallel @Maybe Sha@ threaded through every caller.
-}
data Transport = Local | Ssh Host Sha

{- | The shell command process-compose runs for one node. Local
nodes use the same @just --no-deps@ invocation as before
(preserved verbatim from 'CI.Justfile.recipeCommand'); SSH nodes
emit the three-connection bundle+clone+run dance.
-}
commandFor :: Transport -> RecipeName -> Text
commandFor Local r = recipeCommand r
commandFor (Ssh host sha) r = sshCommand host sha r

{- | The full SSH wrapper for one remote recipe execution. The shape:

  1. @T=$(ssh HOST mktemp -d)@ — captures a fresh remote tempdir.
  2. @git bundle create -@ piped through SSH to materialise the
     bundle on the remote, clone it, and check out the pinned SHA.
  3. @ssh HOST "cd ... && just --no-deps <recipe>"@ — the actual
     build. Its exit code is captured.
  4. @ssh HOST "rm -rf $T"@ — cleanup, always runs.
  5. @exit $rc@ — propagate the build's exit code as the node's.

@-T@ on every SSH suppresses TTY allocation so binary stdin (the
bundle) survives unmolested.
-}
sshCommand :: Host -> Sha -> RecipeName -> Text
sshCommand host sha recipe =
    T.intercalate
        " && "
        [ "T=$(ssh -T " <> h <> " mktemp -d)"
        , "git bundle create - --all 2>/dev/null | ssh -T "
            <> h
            <> " \"cat > $T/repo.bundle && cd $T && git clone --quiet repo.bundle src && cd src && git -c advice.detachedHead=false checkout --quiet "
            <> display sha
            <> "\""
        ]
        <> "; ssh -T "
        <> h
        <> " \"cd $T/src && just --no-deps "
        <> display recipe
        <> "\"; rc=$?; ssh -T "
        <> h
        <> " \"rm -rf $T\"; exit $rc"
  where
    h = display host
