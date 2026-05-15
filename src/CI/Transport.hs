{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | How a 'CI.Node.NodeId' actually executes: locally against the
pipeline's pinned worktree, or remotely against a shared
checkout that's set up once per host per run.

The 'Ssh' transport carries the remote-execution context (host,
SHA, target platform); whether a given remote node is the
per-platform setup node or a recipe node is read off the
'CI.Node.NodeId' inside 'commandFor'. That keeps the setup-vs-
recipe discrimination at one site (here) instead of being decided
once in 'CI.Pipeline' to pick a constructor and again here to
pick a branch.

Remote setup nodes ship the @just@ derivation, bundle @HEAD@
across, and clone into
@~\/.cache\/ci\/\<short-sha\>\/\<platform\>\/src@. Idempotent:
same-SHA reruns hit the cached directory and skip the bundle.
Remote recipe nodes @cd@ into the same shared cached directory
(which their @depends_on@ setup node has already populated) and
run the realised @just --no-deps \<recipe\>@.

The split collapses N bundle transfers (one per recipe per
platform) down to one per remote per run, and to zero on cache
hits.

Every remote command runs over plain @ssh -T \<host\>@. Anything
the local @ssh@ config knows how to dial works as the host
string — bare hostnames, @user\@host@, aliases from
@~\/.ssh\/config@ (incus instances are reached via an ssh alias).
-}
module CI.Transport (
    Transport (..),
    commandFor,
    remoteRunner,
    cachedRunDir,
)
where

import CI.Git (Sha)
import CI.Hosts (Host)
import CI.Justfile (RecipeName, recipeCommand)
import CI.Nix (realisedJust, shipJustDrv)
import CI.Node (NodeId (..))
import CI.NodeKind (isSetupNode)
import CI.Platform (Platform)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)

{- | Where a node runs:

  * 'Local' — process-compose's working_dir is already the pinned
    worktree, so the @just@ subprocess just runs in cwd.
  * 'Ssh' — over @ssh -T \<host\>@ against the shared cached
    checkout under @\$HOME\/.cache\/ci\/\<short-sha\>\/\<platform\>@.
    Whether this node is a setup node or a recipe node is
    discriminated in 'commandFor' off the 'NodeId'.
-}
data Transport
    = Local
    | Ssh Host Sha Platform

{- | The shell command process-compose runs for one node. Local
nodes go through 'CI.Justfile.recipeCommand'; remote nodes
dispatch on whether the 'NodeId' is the synthetic setup node
('isSetupNode') — setup emits the drv-copy + bundle + clone
dance, recipe nodes emit just a cd + realise + run.

The setup-vs-recipe discrimination lives here at the single site
that emits the corresponding shell text, so a new node kind
can't compile cleanly with one branch updated and the other
forgotten.
-}
commandFor :: Transport -> NodeId -> Text
commandFor Local node = recipeCommand node.recipe
commandFor (Ssh host sha targetPlat) node
    | isSetupNode node = setupCommand host sha targetPlat
    | otherwise = recipeRemoteCommand host sha targetPlat node.recipe

{- | The shell-tokens prefix that runs a command on this 'Host':
@ssh -T \<host\>@. @-T@ suppresses TTY allocation so binary stdin
(the @git bundle@ stream) survives unmolested. Anything the local
@ssh@ config knows how to dial — bare @hostname@, @user\@host@,
an alias from @~\/.ssh\/config@ — works as the host string.
-}
remoteRunner :: Host -> Text
remoteRunner host = "ssh -T " <> display host

{- | The shared checkout path on the remote, deterministic from
@(short-sha, platform)@: @\$HOME\/.cache\/ci\/\<short-sha\>\/\<platform\>@.
The setup node clones into @\<cachedRunDir\>\/src@; recipe nodes
@cd@ into the same path.

Across runs against the same SHA the directory persists, so
re-runs (e.g. @--from ci-only@) skip the bundle+clone entirely.
Garbage collection is the user's job — @rm -rf ~/.cache/ci@ when
disk pressure warrants.
-}
cachedRunDir :: Sha -> Platform -> Text
cachedRunDir sha targetPlat =
    "$HOME/.cache/ci/" <> T.take 7 (display sha) <> "/" <> display targetPlat

{- | Setup-node command. Ships the @just@ derivation, then bundles
@HEAD@ across and clones into 'cachedRunDir'. Idempotent: if the
target @src\/@ already exists (cache hit on same-SHA rerun), the
incoming bundle bytes are discarded and the setup exits 0
immediately.

The bundle is always piped over the wire (we don't probe-then-ship
because that'd add a round trip on every run). On a cache hit
the wasted bandwidth is a few MB of bundle bytes discarded into
@/dev/null@ on the remote — fine.
-}
setupCommand :: Host -> Sha -> Platform -> Text
setupCommand host sha targetPlat =
    shipJustDrv r targetPlat
        <> " && git bundle create - --all 2>/dev/null | "
        <> r
        <> " '"
        <> remoteSetupShell sha targetPlat
        <> "'"
  where
    r = remoteRunner host

{- | The remote-side shell snippet the setup node sends over ssh.
Single-quoted on the way through so the local shell leaves @$DIR@
and friends alone; the remote shell expands them. Cache-hit path
short-circuits with @cat > /dev/null@ to consume the bundle bytes
the local side is already piping.
-}
remoteSetupShell :: Sha -> Platform -> Text
remoteSetupShell sha targetPlat =
    T.intercalate "; " $
        [ "set -e"
        , "DIR=" <> cachedRunDir sha targetPlat
        ]
            <> [ "if [ -d \"$DIR/src\" ]; then cat > /dev/null; exit 0; fi"
               , "mkdir -p \"$DIR\""
               , "cd \"$DIR\""
               , "cat > repo.bundle"
               , "git clone --quiet repo.bundle src"
               , "cd src"
               , "git -c advice.detachedHead=false checkout --quiet " <> display sha
               ]

{- | Per-recipe remote command. The corresponding setup node has
already provisioned the cached checkout (process-compose's
@depends_on@ enforces ordering); the recipe just @cd@s into it
and runs the realised @just@.
-}
recipeRemoteCommand :: Host -> Sha -> Platform -> RecipeName -> Text
recipeRemoteCommand host sha targetPlat r' =
    runner
        <> " 'cd "
        <> cachedRunDir sha targetPlat
        <> "/src && "
        <> realisedJust targetPlat r'
        <> "'"
  where
    runner = remoteRunner host
