{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Orchestration for the two run modes (local / strict), the runtime
-- artifact layout under @\$PWD\/.ci\/@, and the just-graph-to-YAML build.
-- "Main" is the dispatch layer; everything mode-specific or orchestration-
-- shaped lives here.
module CI.Pipeline
  ( RunDir (..),
    RunMode (..),
    ensureRunDir,
    runLocal,
    runStrict,
    buildProcessCompose,
  )
where

import CI.CommitStatus (postStatusFor, seedPending)
import CI.Gh (viewRepo)
import CI.Git (ensureCleanTree, resolveSha, withSnapshotWorktree)
import CI.Graph (lowerToRunnerGraph, reachableSubgraph)
import CI.Justfile (fetchDump, recipeCommand)
import CI.LogPath (logDirFor, logPathFor)
import CI.ProcessCompose (ProcessCompose, UpInvocation (..), processNames, runProcessCompose, toProcessCompose)
import CI.ProcessCompose.Events (ProcessState, subscribeStates)
import CI.Root (findRoot)
import CI.Verdict (exitWithVerdict, newOutcomes, recordOutcome)
import Control.Concurrent.Async (link, wait, withAsync)
import Control.Monad (void)
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (die)
import System.FilePath ((</>))

-- | The runtime artifact paths under @\$PWD\/.ci\/@. Built once at the top
-- of a run so 'runLocal' and 'runStrict' both reference the same
-- convention instead of hand-rolling @runDir \<\/\> "pc.log"@ at each
-- call site.
data RunDir = RunDir
  { worktreePath :: FilePath,
    sock :: FilePath,
    pcLog :: FilePath
  }

-- | Create @\$PWD\/.ci\/@ (if missing) and return the canonical sub-paths.
-- Everything we write at runtime lives here so the user gitignores
-- @\/.ci\/@ once and forgets about it.
ensureRunDir :: IO RunDir
ensureRunDir = do
  cwd <- getCurrentDirectory
  let dir = cwd </> ".ci"
  createDirectoryIfMissing True dir
  pure
    RunDir
      { worktreePath = dir </> "worktree",
        sock = dir </> "pc.sock",
        pcLog = dir </> "pc.log"
      }

-- | Local mode: live working tree, no GitHub status posts, no per-recipe
-- log routing. The observer still runs — its only consumer is the
-- verdict accumulator, which gives developer runs the same end-of-run
-- summary strict mode produces. Process-compose's log goes to
-- @.ci\/pc.log@ so even local runs don't leak into @\$TMPDIR@; the same
-- UDS at @.ci\/pc.sock@ is bound so the API surface is available for
-- future consumers (e.g. an MCP server).
runLocal :: RunDir -> [String] -> IO ()
runLocal dirs passthrough = do
  pc <- buildProcessCompose LocalRun
  outcomes <- newOutcomes (processNames pc)
  withObserver dirs.sock (recordOutcome outcomes) $
    void $
      runProcessCompose (UpInvocation dirs.sock dirs.pcLog passthrough) pc
  exitWithVerdict outcomes

-- | Strict mode: clean-tree refuse → resolve repo + SHA → snapshot HEAD
-- via @git worktree@ at @.ci\/worktree@ → start process-compose with its
-- API on @.ci\/pc.sock@ → subscribe to state events, post commit
-- statuses, and accumulate the per-recipe outcome map concurrently
-- with the pipeline run.
--
-- Per-recipe stdout/stderr is split into @.ci\/\<sha\>\/\<recipe\>.log@
-- (created here before process-compose spawns) so each GitHub commit
-- status can carry a navigable path to the matching log. The SHA-keyed
-- directory keeps history across runs: a green-then-red sequence on the
-- same checkout leaves both runs' logs side-by-side under @.ci\/@.
--
-- The two consumers of the state stream — 'postStatusFor' (GitHub
-- write) and 'recordOutcome' (local accumulator) — are composed at
-- this single call site rather than entangled inside the observer or
-- the GH-posting code. Both share
-- 'CI.ProcessCompose.Events.psToTerminalStatus' as the underlying
-- terminal-state classifier, so the GH check page and the local
-- verdict agree on which recipes succeeded.
--
-- Process-compose's own exit code is intentionally ignored — with
-- @restart: no@ on every process it no longer reflects pipeline
-- outcome (a failed recipe leaves pc exiting 0). The accumulated
-- outcome map is the source of truth; 'exitWithVerdict' derives the
-- final 'ExitCode' from it.
runStrict :: RunDir -> [String] -> IO ()
runStrict dirs passthrough = do
  dieOnLeft =<< ensureCleanTree
  repo <- dieOnLeft =<< viewRepo
  sha <- dieOnLeft =<< resolveSha
  let logDir = logDirFor sha
  withSnapshotWorktree dirs.worktreePath $ do
    createDirectoryIfMissing True logDir
    pc <- buildProcessCompose $ StrictRun dirs.worktreePath logDir
    let recipes = processNames pc
    seedPending repo sha logDir recipes
    outcomes <- newOutcomes recipes
    let onState ps = postStatusFor repo sha logDir ps >> recordOutcome outcomes ps
    withObserver dirs.sock onState $
      void $
        runProcessCompose (UpInvocation dirs.sock dirs.pcLog passthrough) pc
    exitWithVerdict outcomes

-- | Bracket @body@ between a 'subscribeStates' subscription on @sock@
-- and a clean @wait@ on it: spawn the observer, 'link' so its crash
-- aborts the caller, run @body@, then 'wait' for the WebSocket to
-- close (which it does when process-compose exits). The
-- async-lifecycle scaffold lives here so 'runLocal' and 'runStrict'
-- vary only in their @onState@ callback and the body they pass.
withObserver :: FilePath -> (ProcessState -> IO ()) -> IO a -> IO a
withObserver sockP onState body =
  withAsync (subscribeStates sockP onState) $ \obs -> do
    link obs
    result <- body
    wait obs
    pure result

-- | The two pipeline-build modes. 'LocalRun' is the @dev@ / @dump-yaml@
-- shape: no worktree pin, no per-recipe log routing. 'StrictRun'
-- carries the two paths that always travel together — the @git
-- worktree@ snapshot every recipe @chdir@s into, and the
-- @.ci\/\<sha\>\/@ log directory the YAML emitter routes each
-- process's stdout/stderr to. A sum type instead of two parallel
-- @Maybe FilePath@s rules out the mixed @(Just, Nothing)@ /
-- @(Nothing, Just)@ states that produce logically inconsistent YAML.
data RunMode
  = LocalRun
  | -- | @StrictRun worktreeDir logDir@.
    StrictRun FilePath FilePath

-- | Walk @just --dump@ → root → reachable subgraph → topologically
-- lowered DAG → 'ProcessCompose' YAML, parameterised by the run mode.
-- The two per-process knobs ('workingDir' and 'logLocation') each
-- pattern-match on 'RunMode' directly — the sum-type discipline reaches
-- the YAML emitter rather than being unpacked into a @(Maybe, Maybe)@
-- pair one line up.
buildProcessCompose :: RunMode -> IO ProcessCompose
buildProcessCompose mode = do
  recipes <- dieOnLeft =<< fetchDump
  root <- dieOnLeft $ findRoot recipes
  reachable <- dieOnLeft $ reachableSubgraph root recipes
  graph <- dieOnLeft $ lowerToRunnerGraph reachable
  pure $ toProcessCompose (workingDir mode) recipeCommand (logLocation mode) graph
  where
    workingDir LocalRun = Nothing
    workingDir (StrictRun wt _) = Just wt
    logLocation LocalRun = const Nothing
    logLocation (StrictRun _ ld) = Just . logPathFor ld

-- | The single 'die' site in the project: every recoverable failure
-- mode threads up through @Either e a@ to this boundary, where the
-- structured error's 'Display' rendering becomes the exit message.
--
-- Shape note: takes @Either e a@ rather than @IO (Either e a)@ so the
-- same helper works for both pure Eithers (@dieOnLeft $ findRoot
-- recipes@) and IO ones (@dieOnLeft =<< ensureCleanTree@). A helper
-- typed to @IO (Either e a) -> IO a@ would force every pure call site
-- to add a @pure@.
dieOnLeft :: Display e => Either e a -> IO a
dieOnLeft = either (die . T.unpack . display) pure
