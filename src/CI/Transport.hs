{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | How a node actually executes: locally against the pipeline's
-- pinned worktree, or remotely against a shared checkout that's
-- set up once per host per run.
--
-- Two orthogonal axes meet here:
--
--   * 'Transport' — *where* the command runs ('Local' or 'Ssh ...').
--     Varies with connection mechanism (new runner type, different
--     transport).
--   * 'CommandShape' — *what* command this node runs ('SetupCommand'
--     for the per-platform bundle ship, 'RecipeCommand' for the
--     user recipe). Varies with the set of node kinds the
--     orchestrator schedules.
--
-- 'commandFor' takes both. The caller ('CI.Pipeline.commandForNode')
-- already knows which kind it's emitting (it built the setup nodes
-- into the graph) and passes the shape explicitly, classifying the
-- 'NodeId' via 'CI.NodeKind.isSetupNode'; 'Transport' no longer has
-- to re-derive the node kind from a 'NodeId'. The cross-module
-- invariant — that exactly one site discriminates setup vs recipe —
-- lives at that 'commandForNode' call site, not in 'Transport'.
--
-- Remote setup nodes ship the @just@ derivation, bundle @HEAD@
-- across, and clone into
-- @~\/.cache\/ci\/\<short-sha\>\/\<platform\>\/src@. Idempotent:
-- same-SHA reruns hit the cached directory and skip the bundle.
-- Remote recipe nodes @cd@ into the same shared cached directory
-- (which their @depends_on@ setup node has already populated) and
-- run the realised @just --no-deps \<recipe\>@.
--
-- The split collapses N bundle transfers (one per recipe per
-- platform) down to one per remote per run, and to zero on cache
-- hits.
--
-- Every remote command runs over plain @ssh -T \<host\>@. Anything
-- the local @ssh@ config knows how to dial works as the host
-- string — bare hostnames, @user\@host@, aliases from
-- @~\/.ssh\/config@ (incus instances are reached via an ssh alias).
module CI.Transport
  ( Transport (..),
    CommandShape (..),
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

-- | Where a node runs:
--
--   * 'Local' — process-compose's working_dir is already the pinned
--     worktree, so the @just@ subprocess just runs in cwd.
--   * 'Ssh' — over @ssh -T \<host\>@ against the shared cached
--     checkout under @\$HOME\/.cache\/ci\/\<short-sha\>\/\<platform\>@.
--
-- Setup-vs-recipe selection is *not* encoded here — that's the
-- 'CommandShape' axis, passed alongside.
data Transport
  = Local
  | Ssh Host Sha Platform

-- | What command this node runs, independent of where:
--
--   * 'SetupCommand' — the per-platform drv-copy + bundle + clone
--     dance. Only meaningful on an 'Ssh' transport; on 'Local'
--     there's nothing to ship.
--   * 'RecipeCommand' — invoke the named user recipe via @just@.
--
-- Built explicitly by 'CI.Pipeline.commandForNode' from the
-- fanout it already constructed, so 'commandFor' doesn't re-derive
-- the choice from node identity.
data CommandShape
  = SetupCommand
  | RecipeCommand RecipeName

-- | The shell command process-compose runs for one node. The two
-- axes are independent:
--
--   * @(Local, RecipeCommand r)@ — bare @just@ invocation in cwd.
--   * @(Local, SetupCommand)@ — no-op; locally there's nothing to
--     ship. We surface this as an explicit error rather than a
--     silent empty command so a misuse upstream is loud, not
--     silently broken.
--   * @(Ssh ..., SetupCommand)@ — drv-copy + bundle + clone.
--   * @(Ssh ..., RecipeCommand r)@ — cd into the cached checkout
--     and run the realised @just@.
--
-- The 'Local'+'SetupCommand' impossibility comes from the
-- fanout: setup nodes are emitted only for remote platforms (see
-- 'CI.Pipeline.fanOut'), so 'CI.Pipeline.commandForNode' only ever
-- pairs 'SetupCommand' with 'Ssh'. The 'error' is a contract
-- guard, not a runtime branch.
commandFor :: Transport -> CommandShape -> Text
commandFor Local (RecipeCommand r) = recipeCommand r
commandFor Local SetupCommand =
  error "internal error: SetupCommand on Local transport (setup nodes are emitted only for remote platforms)"
commandFor (Ssh host sha targetPlat) SetupCommand =
  setupCommand host sha targetPlat
commandFor (Ssh host sha targetPlat) (RecipeCommand r) =
  recipeRemoteCommand host sha targetPlat r

-- | The shell-tokens prefix that runs a command on this 'Host':
-- @ssh -T \<host\>@. @-T@ suppresses TTY allocation so binary stdin
-- (the @git bundle@ stream) survives unmolested. Anything the local
-- @ssh@ config knows how to dial — bare @hostname@, @user\@host@,
-- an alias from @~\/.ssh\/config@ — works as the host string.
remoteRunner :: Host -> Text
remoteRunner host = "ssh -T " <> display host

-- | The shared checkout path on the remote, deterministic from
-- @(short-sha, platform)@: @\$HOME\/.cache\/ci\/\<short-sha\>\/\<platform\>@.
-- The setup node clones into @\<cachedRunDir\>\/src@; recipe nodes
-- @cd@ into the same path.
--
-- Across runs against the same SHA the directory persists, so
-- re-runs (e.g. @--from ci-only@) skip the bundle+clone entirely.
-- Garbage collection is the user's job — @rm -rf ~/.cache/ci@ when
-- disk pressure warrants.
cachedRunDir :: Sha -> Platform -> Text
cachedRunDir sha targetPlat =
  "$HOME/.cache/ci/" <> T.take 7 (display sha) <> "/" <> display targetPlat

-- | Setup-node command. Ships the @just@ derivation, then bundles
-- @HEAD@ across and clones into 'cachedRunDir'. Idempotent: if the
-- target @src\/@ already exists (cache hit on same-SHA rerun), the
-- incoming bundle bytes are discarded and the setup exits 0
-- immediately.
--
-- The bundle is always piped over the wire (we don't probe-then-ship
-- because that'd add a round trip on every run). On a cache hit
-- the wasted bandwidth is a few MB of bundle bytes discarded into
-- @/dev/null@ on the remote — fine.
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

-- | The remote-side shell snippet the setup node sends over ssh.
-- Single-quoted on the way through so the local shell leaves @$DIR@
-- and friends alone; the remote shell expands them. Cache-hit path
-- short-circuits with @cat > /dev/null@ to consume the bundle bytes
-- the local side is already piping.
remoteSetupShell :: Sha -> Platform -> Text
remoteSetupShell sha targetPlat =
  T.intercalate "; " $
    [ "set -e",
      "DIR=" <> cachedRunDir sha targetPlat
    ]
      <> [ "if [ -d \"$DIR/src\" ]; then cat > /dev/null; exit 0; fi",
           "mkdir -p \"$DIR\"",
           "cd \"$DIR\"",
           "cat > repo.bundle",
           "git clone --quiet repo.bundle src",
           "cd src",
           "git -c advice.detachedHead=false checkout --quiet " <> display sha
         ]

-- | Per-recipe remote command. The corresponding setup node has
-- already provisioned the cached checkout (process-compose's
-- @depends_on@ enforces ordering); the recipe just @cd@s into it
-- and runs the realised @just@.
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
