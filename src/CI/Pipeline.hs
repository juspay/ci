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
import CI.Git (Sha, ensureCleanTree, resolveSha, withSnapshotWorktree)
import CI.Graph (lowerToRunnerGraph, reachableSubgraph)
import CI.Hosts (Hosts, hostsPath, loadHosts, lookupHost, promptAndPersistHost)
import CI.Justfile (Attribute (..), Recipe (..), RecipeName, fetchDump)
import CI.LogPath (logDirFor, logPathFor, platformDir)
import CI.Node (NodeId (..), parseNodeId)
import CI.Platform (Platform, localPlatform, osToPlatform)
import CI.ProcessCompose (ProcessCompose, UpInvocation (..), processNames, runProcessCompose, toProcessCompose)
import CI.ProcessCompose.Events (ProcessState (..), subscribeStates)
import CI.Root (findRoot)
import CI.Transport (Transport (..), commandFor)
import CI.Verdict (exitWithVerdict, newOutcomes, recordOutcome)
import Control.Concurrent.Async (link, wait, withAsync)
import Control.Monad (foldM, void)
import Data.Foldable (for_)
import Data.List (nub)
import qualified Data.Map.Strict as Map
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
            }

{- | Local mode: live working tree, no GitHub status posts, no per-recipe
log routing. The observer still runs — its only consumer is the
verdict accumulator, which gives developer runs the same end-of-run
summary strict mode produces. Process-compose's log goes to
@.ci\/pc.log@ so even local runs don't leak into @\$TMPDIR@; the same
UDS at @.ci\/pc.sock@ is bound so the API surface is available for
future consumers (e.g. an MCP server).

SSH lanes are supported in local mode too: any non-local platform in
the pipeline triggers host resolution (prompt-on-miss, persist to
@~\/.config\/ci\/hosts.json@) and an SSH-shaped @command@ that
bundles @HEAD@ across rather than the dirty live tree. The dev's
uncommitted work is intentionally invisible to remote lanes — the
bundle reflects committed history only.
-}
runLocal :: RunDir -> [String] -> IO ()
runLocal dirs passthrough = do
    pc <- buildProcessCompose LocalRun
    outcomes <- newOutcomes (processNames pc)
    let onState ps = forNode ps $ \node -> recordOutcome outcomes node ps
    withObserver dirs.sock onState $
        void $
            runProcessCompose (UpInvocation dirs.sock dirs.pcLog passthrough) pc
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
runStrict :: RunDir -> [String] -> IO ()
runStrict dirs passthrough = do
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
        let onState ps = forNode ps $ \node ->
                postStatusFor repo sha logDir node ps
                    >> recordOutcome outcomes node ps
        withObserver dirs.sock onState $
            void $
                runProcessCompose (UpInvocation dirs.sock dirs.pcLog passthrough) pc
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

{- | The single 'parseNodeId' site in the runtime path. Both observers
('postStatusFor' and 'recordOutcome') consume the same parsed
'NodeId', so "is this event for a node we scheduled?" is decided
once instead of being re-decided in each downstream module. If
'parseNodeId' rejects the event name, the action is silently
dropped — same policy both sides had locally, now unified.
-}
forNode :: ProcessState -> (NodeId -> IO ()) -> IO ()
forNode ps action = for_ (parseNodeId ps.name) action

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

{- | Walk @just --dump@ → root → reachable subgraph → topologically
lowered DAG → fan out across the pipeline's platform set →
'ProcessCompose' YAML. Platform discovery, host resolution, and
transport selection all happen here so the YAML emitter
("CI.ProcessCompose") stays a dumb encoder.

 * Pipeline platforms come from the root recipe's OS attributes
   (@[linux] [macos] [metadata(\"ci\")] root:@). A root with no
   OS attrs defaults to the local platform only.

 * Per-platform host resolution loads @~\/.config\/ci\/hosts.json@
   once. In 'LocalRun' a missing host is prompted for and
   persisted; in 'StrictRun' a missing host dies before the
   pipeline starts (no TTY mid-run).

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
    let pipelinePlatforms = pipelinePlatformsFor rootRecipe localPlat
        nodeGraph = fanOut pipelinePlatforms recipeGraph
        hasRemote = any (/= localPlat) pipelinePlatforms
    hosts <- resolveHostsFor mode localPlat pipelinePlatforms
    -- Only the SSH branch in 'CI.Transport.commandFor' needs a SHA, so
    -- we shell out to git only when at least one lane is remote. This
    -- keeps @dump-yaml@ for a single-platform pipeline working outside
    -- a git checkout (the original behaviour).
    shaForTransport <-
        if hasRemote
            then RemoteLanes <$> (dieOnLeft =<< resolveSha)
            else pure NoRemoteLanes
    let mkCommand = commandForNode shaForTransport localPlat hosts
    pure $ toProcessCompose (workingDir mode) mkCommand (logLocation mode) nodeGraph
  where
    workingDir LocalRun = Nothing
    workingDir (StrictRun wt _) = Just wt
    logLocation LocalRun = const Nothing
    logLocation (StrictRun _ ld) = Just . logPathFor ld

{- | The pipeline's platform set, derived from the root recipe's
@[linux] [macos] ...@ attributes. A root with no OS attrs falls
back to @[localPlatform]@ — a single-host pipeline behaves
identically to the pre-fanout shape. Duplicates are removed but
order is preserved (attribute order in the source).
-}
pipelinePlatformsFor :: Recipe -> Platform -> [Platform]
pipelinePlatformsFor root localPlat =
    case nub [p | Os o <- root.attributes, Just p <- [osToPlatform o]] of
        [] -> [localPlat]
        ps -> ps

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

{- | Load the hosts config and ensure every remote platform the
pipeline targets has a host. In 'LocalRun' we prompt on miss and
persist; in 'StrictRun' the runner refuses to start without
complete config so a missing host doesn't tear the run down mid-way.
-}
resolveHostsFor :: RunMode -> Platform -> [Platform] -> IO Hosts
resolveHostsFor mode localPlat platforms = do
    hosts0 <- loadHosts
    let remotes = filter (/= localPlat) platforms
    case mode of
        LocalRun -> foldM addInteractively hosts0 remotes
        StrictRun{} -> do
            let missing = [p | p <- remotes, Nothing <- [lookupHost p hosts0]]
            case missing of
                [] -> pure hosts0
                ms -> do
                    path <- hostsPath
                    die $
                        "strict mode (CI=true) requires preconfigured hosts in "
                            <> path
                            <> "; missing entries for: "
                            <> T.unpack (T.intercalate ", " (display <$> ms))
  where
    addInteractively hs p = snd <$> promptAndPersistHost p hs

{- | The pipeline's SHA-resolution state, expressed as one value
instead of a 'Maybe Sha' that has to agree with an independent
'hasRemote' boolean. 'NoRemoteLanes' is the @dump-yaml@-outside-a-repo
case; 'RemoteLanes' carries the SHA every SSH lane needs to clone.

Modelling it as a sum rather than a 'Maybe' kills the previously-
unreachable @(SSH lane, Nothing)@ branch — the type now witnesses
that "we have remote lanes" and "we have a SHA" are the same fact.
-}
data ShaForTransport = NoRemoteLanes | RemoteLanes Sha

{- | Per-node command construction: 'Local' if the node's platform
matches the runner; otherwise 'Ssh' against the resolved host.

The two error branches witness the fan-out invariants:
'resolveHostsFor' ensures every remote platform has a host, and
'buildProcessCompose' produces 'RemoteLanes' iff a remote lane exists.
A remote 'NodeId' with 'NoRemoteLanes' would be a 'fanOut' bug — the
caller shouldn't be able to build that state, so we 'error' rather
than carry a defensive 'Maybe' through the type.
-}
commandForNode :: ShaForTransport -> Platform -> Hosts -> NodeId -> T.Text
commandForNode shaForTransport localPlat hosts node
    | node.platform == localPlat = commandFor Local node.recipe
    | otherwise = case (lookupHost node.platform hosts, shaForTransport) of
        (Just h, RemoteLanes sha) -> commandFor (Ssh h sha) node.recipe
        (Nothing, _) ->
            error $
                "internal error: no SSH host for "
                    <> T.unpack (display node.platform)
                    <> " (resolveHostsFor should have caught this)"
        (_, NoRemoteLanes) ->
            error "internal error: remote NodeId reached commandForNode with NoRemoteLanes (fanOut contract violated)"

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
