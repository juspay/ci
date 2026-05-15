{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Orchestration for the two run modes (local / strict), the runtime
artifact layout under @\$PWD\/.ci\/@, and the just-graph-to-YAML build.
"Main" is the dispatch layer; everything mode-specific or orchestration-
shaped lives here.
-}
module CI.Pipeline (
    RunDir (..),
    RunMode (..),
    ensureRunDir,
    runLocal,
    runStrict,
    buildProcessCompose,
)
where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.CommitStatus (postStatusFor, seedPending)
import CI.Gh (viewRepo)
import CI.Git (Sha, ensureCleanTree, resolveSha, shaPlaceholder, withSnapshotWorktree)
import CI.Graph (lowerToRunnerGraph, reachableSubgraph)
import CI.Hosts (Hosts, hostsPlatforms, loadHosts, lookupHost)
import CI.Justfile (Attribute (..), Recipe (..), RecipeName, fetchDump)
import qualified CI.Justfile as J
import CI.LogPath (logDirFor, logPathFor, platformDir)
import CI.Node (NodeId (..), parseNodeId)
import CI.Platform (Platform, localPlatform, osToPlatforms, platformOs)
import CI.ProcessCompose (ProcessCompose, UpInvocation (..), processNames, runProcessCompose, toProcessCompose)
import CI.ProcessCompose.Events (ProcessState (..), subscribeStates)
import CI.Root (findRoot)
import CI.Transport (Transport (Local), commandFor, sshTransport)
import CI.Verdict (exitWithVerdict, newOutcomes, recordOutcome)
import Control.Concurrent.Async (link, wait, withAsync)
import Control.Monad (void)
import Data.Foldable (for_)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (die)
import System.FilePath ((</>))

{- | The runtime artifact paths under @\$PWD\/.ci\/@. Built once at the top
of a run so 'runLocal' and 'runStrict' both reference the same
convention instead of hand-rolling @runDir \<\/\> "pc.log"@ at each
call site.
-}
data RunDir = RunDir
    { worktreePath :: FilePath
    , sock :: FilePath
    , pcLog :: FilePath
    , pcYaml :: FilePath
    }

{- | Create @\$PWD\/.ci\/@ (if missing) and return the canonical sub-paths.
Everything we write at runtime lives here so the user gitignores
@\/.ci\/@ once and forgets about it.
-}
ensureRunDir :: IO RunDir
ensureRunDir = do
    cwd <- getCurrentDirectory
    let dir = cwd </> ".ci"
    createDirectoryIfMissing True dir
    pure
        RunDir
            { worktreePath = dir </> "worktree"
            , sock = dir </> "pc.sock"
            , pcLog = dir </> "pc.log"
            , pcYaml = dir </> "pc.yaml"
            }

{- | Local mode: live working tree, no GitHub status posts, no per-recipe
log routing. The observer still runs — its only consumer is the
verdict accumulator, which gives developer runs the same end-of-run
summary strict mode produces. Process-compose's log goes to
@.ci\/pc.log@ so even local runs don't leak into @\$TMPDIR@; the same
UDS at @.ci\/pc.sock@ is bound so the API surface is available for
future consumers (e.g. an MCP server).

SSH lanes are supported in local mode too: any non-local platform
in the pipeline requires a @~\/.config\/ci\/hosts.json@ entry (the
user opts in by editing the file; missing entries are excluded
from the fanout by 'pipelinePlatformsFor'). Each remote lane gets
an SSH-shaped @command@ that bundles @HEAD@ across rather than
the dirty live tree — the dev's uncommitted work is intentionally
invisible to remote lanes; the bundle reflects committed history
only.
-}
runLocal :: RunDir -> Bool -> [String] -> IO ()
runLocal dirs tui passthrough = do
    pc <- buildProcessCompose LocalRun
    outcomes <- newOutcomes (processNames pc)
    let onState ps = withParsedNode ps $ \node -> recordOutcome outcomes node ps
    withObserver dirs.sock onState $
        void $
            runProcessCompose (UpInvocation dirs.sock dirs.pcLog dirs.pcYaml tui passthrough) pc
    exitWithVerdict outcomes

{- | Strict mode: clean-tree refuse → resolve repo + SHA → snapshot HEAD
via @git worktree@ at @.ci\/worktree@ → start process-compose with its
API on @.ci\/pc.sock@ → subscribe to state events, post commit
statuses, and accumulate the per-node outcome map concurrently with
the pipeline run.

Per-node stdout/stderr is split into
@.ci\/\<sha\>\/\<platform\>\/\<recipe\>.log@ (created here before
process-compose spawns) so each GitHub commit status can carry a
navigable path to the matching log. The SHA-keyed directory keeps
history across runs: a green-then-red sequence on the same checkout
leaves both runs' logs side-by-side under @.ci\/@.

The two consumers of the state stream — 'postStatusFor' (GitHub
write) and 'recordOutcome' (local accumulator) — are composed at
this single call site rather than entangled inside the observer or
the GH-posting code. Both share
'CI.ProcessCompose.Events.psToTerminalStatus' as the underlying
terminal-state classifier, so the GH check page and the local
verdict agree on which nodes succeeded.

Process-compose's own exit code is intentionally ignored — with
@restart: no@ on every process it no longer reflects pipeline
outcome (a failed node leaves pc exiting 0). The accumulated
outcome map is the source of truth; 'exitWithVerdict' derives the
final 'ExitCode' from it.
-}
runStrict :: RunDir -> Bool -> [String] -> IO ()
runStrict dirs tui passthrough = do
    dieOnLeft =<< ensureCleanTree
    repo <- dieOnLeft =<< viewRepo
    sha <- dieOnLeft =<< resolveSha
    let logDir = logDirFor sha
    withSnapshotWorktree dirs.worktreePath $ do
        pc <- buildProcessCompose $ StrictRun dirs.worktreePath logDir
        let nodes = processNames pc
        createPlatformDirs logDir nodes
        seedPending repo sha logDir nodes
        outcomes <- newOutcomes nodes
        let onState ps = withParsedNode ps $ \node ->
                postStatusFor repo sha logDir node ps
                    >> recordOutcome outcomes node ps
        withObserver dirs.sock onState $
            void $
                runProcessCompose (UpInvocation dirs.sock dirs.pcLog dirs.pcYaml tui passthrough) pc
        exitWithVerdict outcomes

{- | Materialise every @.ci\/\<sha\>\/\<platform\>\/@ subdirectory the
pipeline will route logs to, before process-compose spawns. pc
creates the per-recipe log *file* itself but won't create
intermediate directories — without this the first event for a
platform whose subdir doesn't exist fails the spawn.
-}
createPlatformDirs :: FilePath -> [NodeId] -> IO ()
createPlatformDirs logDir nodes =
    mapM_ (createDirectoryIfMissing True . platformDir logDir) (nub [n.platform | n <- nodes])

{- | Enforce the wire-event-identity invariant at the single site that
owns it: parse @ps.name@ as a 'NodeId' and run @action@ only if it
names a node we scheduled. Both observers ('postStatusFor' and
'recordOutcome') consume the resulting parsed 'NodeId', so the
drop-on-unparseable policy is decided once here instead of being
re-decided in each downstream module. The name signals the
parse/filter responsibility — this is the gate, not a bare iteration.
-}
withParsedNode :: ProcessState -> (NodeId -> IO ()) -> IO ()
withParsedNode ps action = for_ (parseNodeId ps.name) action

{- | Bracket @body@ between a 'subscribeStates' subscription on @sock@
and a clean @wait@ on it: spawn the observer, 'link' so its crash
aborts the caller, run @body@, then 'wait' for the WebSocket to
close (which it does when process-compose exits). The
async-lifecycle scaffold lives here so 'runLocal' and 'runStrict'
vary only in their @onState@ callback and the body they pass.
-}
withObserver :: FilePath -> (ProcessState -> IO ()) -> IO a -> IO a
withObserver sockP onState body =
    withAsync (subscribeStates sockP onState) $ \obs -> do
        link obs
        result <- body
        wait obs
        pure result

{- | The two pipeline-build modes. 'LocalRun' is the @dev@ / @dump-yaml@
shape: no worktree pin, no per-recipe log routing. 'StrictRun'
carries the two paths that always travel together — the @git
worktree@ snapshot every local recipe @chdir@s into, and the
@.ci\/\<sha\>\/@ log directory the YAML emitter routes each
process's stdout/stderr to. A sum type instead of two parallel
@Maybe FilePath@s rules out the mixed @(Just, Nothing)@ /
@(Nothing, Just)@ states that produce logically inconsistent YAML.
-}
data RunMode
    = LocalRun
    | -- | @StrictRun worktreeDir logDir@.
      StrictRun FilePath FilePath
    | {- | YAML-inspection mode for @dump-yaml@: no working dir, no log
      routing, and (importantly) no host resolution side effects.
      Missing 'CI.Hosts.Host' entries are tolerated; SSH-lane
      commands render with a placeholder so the structural keys
      (process names, depends_on edges) still reflect the real
      fanout. Used by the macos remote's smoke test where stdin is
      closed and prompting would deadlock.
      -}
      DumpRun

{- | The two YAML-shape projections of 'RunMode': the working
directory every local recipe @chdir@s into, and the per-node log
location the YAML emitter routes stdout/stderr to. Both vary
together across modes — 'StrictRun' supplies both; 'LocalRun' and
'DumpRun' supply neither — so they live in a single
'RunMode'-pattern-match rather than two parallel where-clauses that
have to stay in lockstep across future 'RunMode' constructors.
-}
yamlPathsFor :: RunMode -> (Maybe FilePath, NodeId -> Maybe FilePath)
yamlPathsFor (StrictRun wt ld) = (Just wt, Just . logPathFor ld)
yamlPathsFor _ = (Nothing, const Nothing)

{- | Walk @just --dump@ → root → reachable subgraph → topologically
lowered DAG → fan out across the pipeline's platform set →
'ProcessCompose' YAML. Platform discovery, host resolution, and
transport selection all happen here so the YAML emitter
("CI.ProcessCompose") stays a dumb encoder.

 * Pipeline platforms come from the root recipe's OS attributes
   (@[linux] [macos] [metadata(\"ci\")] root:@). A root with no
   OS attrs defaults to the local platform only.

 * Host resolution loads @~\/.config\/ci\/hosts.json@ once.
   'pipelinePlatformsFor' silently excludes platforms with no
   entry from the fanout, so a missing host is never a runtime
   failure — the user opts in by editing the file.

 * Each fanned-out 'NodeId' gets a 'Local' or @Ssh host@ transport
   depending on whether its platform matches the runner's; the
   'CI.Transport.commandFor' rendering is the only site that knows
   SSH command shapes.
-}
buildProcessCompose :: RunMode -> IO ProcessCompose
buildProcessCompose mode = do
    recipes <- dieOnLeft =<< fetchDump
    rootName <- dieOnLeft $ findRoot recipes
    rootRecipe <- case Map.lookup rootName recipes of
        Just r -> pure r
        -- findRoot guarantees this; the lookup is defensive.
        Nothing -> die $ "internal error: root " <> T.unpack (display rootName) <> " missing from recipe map"
    reachable <- dieOnLeft $ reachableSubgraph rootName recipes
    recipeGraph <- dieOnLeft $ lowerToRunnerGraph reachable
    localPlat <- dieOnLeft localPlatform
    hosts <- loadHosts
    let pipelinePlatforms = pipelinePlatformsFor rootRecipe localPlat hosts
    case pipelinePlatforms of
        [] ->
            die $
                "root recipe declares OS attrs but no matching system is configured. "
                    <> "Either remove the OS attrs from "
                    <> T.unpack (display rootName)
                    <> " or add an entry to ~/.config/ci/hosts.json for one of: "
                    <> unwords (show <$> rootOsFamilies rootRecipe)
        _ -> pure ()
    let nodeGraph = fanOut pipelinePlatforms recipeGraph
        hasRemote = any (/= localPlat) pipelinePlatforms || any (\p -> isJust (lookupHost p hosts)) pipelinePlatforms
    -- @DumpRun@ skips @resolveSha@ and the SSH branch falls back to
    -- 'shaPlaceholder' so @dump-yaml@ works outside a git checkout.
    remoteLaneState <- case mode of
        DumpRun -> pure (RemoteLanes shaPlaceholder)
        _
            | hasRemote -> RemoteLanes <$> (dieOnLeft =<< resolveSha)
            | otherwise -> pure NoRemoteLanes
    let mkCommand = commandForNode remoteLaneState localPlat hosts
        (yamlWorkingDir, yamlLogLocation) = yamlPathsFor mode
    pure $ toProcessCompose yamlWorkingDir mkCommand yamlLogLocation nodeGraph

{- | The pipeline's platform set: the intersection of (the root
recipe's declared OS families) with (the systems we have either a
hosts.json entry for OR are running on locally).

  * Root @[linux] [macos]@ + local @x86_64-linux@ + hosts.json
    @{aarch64-darwin: ...}@ → @{x86_64-linux, aarch64-darwin}@.
  * Root @[linux]@ + local @x86_64-linux@ + empty hosts.json →
    @{x86_64-linux}@ (local-only, no macos).
  * Root with no OS attrs → @{localPlat}@ (single-host shape,
    same as before fanout existed).

A system in @hosts.json@ whose OS family doesn't appear in the root
attributes is silently ignored — the user opts in by adding the OS
attribute to the root. Symmetrically, an OS family in the root that
matches no configured system is silently empty for that family —
the user opts in by adding an entry to @hosts.json@.
-}
pipelinePlatformsFor :: Recipe -> Platform -> Hosts -> [Platform]
pipelinePlatformsFor root localPlat hosts =
    let configured = nub (localPlat : hostsPlatforms hosts)
     in case rootOsFamilies root of
            [] -> [localPlat]
            oss -> filter (\p -> platformOs p `elem` oss) configured

{- | The OS-family attributes declared on a recipe ('[linux]',
'[macos]', etc.), as a plain list. Used both by
'pipelinePlatformsFor' (to compute the fanout set) and by the
empty-fanout error in 'buildProcessCompose' (to tell the user
which families couldn't be satisfied).
-}
rootOsFamilies :: Recipe -> [J.Os]
rootOsFamilies r = [o | Os o <- r.attributes]

{- | Cross-product the recipe DAG with the pipeline's platform set: one
'NodeId' per @(recipe, platform)@, edges replicated lane-by-lane
with no cross-platform connections. The lanes run independently;
a failure on linux doesn't block macos and vice versa (and the
cross-lane failure tolerance ('restart: no', 'exit_on_skipped:
false') already in 'CI.ProcessCompose' carries that through).
-}
fanOut :: [Platform] -> G.AdjacencyMap RecipeName -> G.AdjacencyMap NodeId
fanOut platforms g =
    G.vertices [NodeId r p | r <- G.vertexList g, p <- platforms]
        `G.overlay` G.edges [(NodeId r p, NodeId d p) | (r, d) <- G.edgeList g, p <- platforms]

{- | Orchestrator-side branching: do we have a SHA to clone on the
remote, or are all lanes inline? @DumpRun@ short-circuits to
@RemoteLanes shaPlaceholder@ so inspection works outside a git
checkout; real runs hit @resolveSha@ when at least one lane is
remote.
-}
data RemoteLaneState = NoRemoteLanes | RemoteLanes Sha

{- | Per-node command construction.

  * Hosts-entry present → SSH through that runner (overrides local
    inline execution even when @node.platform == localPlat@). The
    'Ssh' transport carries the target 'Platform' directly; per-arch
    behaviour lives in 'CI.Transport.commandFor' and 'CI.Nix'.

  * No hosts entry, but platform matches local → inline @Local@.

  * No hosts entry, non-local platform → unreachable by the
    'pipelinePlatformsFor' invariant. Every platform in
    @pipelinePlatforms@ either matches local or has a hosts entry
    by construction, so a 'Nothing' on the non-local branch means
    something is wrong upstream.
-}
commandForNode :: RemoteLaneState -> Platform -> Hosts -> NodeId -> T.Text
commandForNode remoteLaneState localPlat hosts node = case lookupHost node.platform hosts of
    Just h -> case remoteLaneState of
        RemoteLanes sha -> commandFor (sshTransport h sha node.platform) node.recipe
        NoRemoteLanes -> shaContractError
    Nothing
        | node.platform == localPlat -> commandFor Local node.recipe
        | otherwise -> hostContractError
  where
    hostContractError =
        error $
            "internal error: no SSH host for "
                <> T.unpack (display node.platform)
                <> " (pipelinePlatformsFor should have excluded this)"
    shaContractError =
        error $
            "internal error: hosts entry for "
                <> T.unpack (display node.platform)
                <> " but no SHA resolved (buildProcessCompose hasRemote logic broken)"

{- | The single 'die' site in the project: every recoverable failure
mode threads up through @Either e a@ to this boundary, where the
structured error's 'Display' rendering becomes the exit message.

Shape note: takes @Either e a@ rather than @IO (Either e a)@ so the
same helper works for both pure Eithers (@dieOnLeft $ findRoot
recipes@) and IO ones (@dieOnLeft =<< ensureCleanTree@). A helper
typed to @IO (Either e a) -> IO a@ would force every pure call site
to add a @pure@.
-}
dieOnLeft :: (Display e) => Either e a -> IO a
dieOnLeft = either (die . T.unpack . display) pure
