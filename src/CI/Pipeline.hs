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

import CI.CommitStatus (logDirFor, logPathFor, postStatusFor, seedPending)
import CI.Root (findRoot)
import CI.Gh (viewRepo)
import CI.Git (ensureCleanTree, resolveSha, withSnapshotWorktree)
import CI.Graph (lowerToRunnerGraph, reachableSubgraph)
import CI.Justfile (RecipeName, fetchDump, recipeCommand)
import CI.ProcessCompose (ProcessCompose, UpInvocation (..), processNames, runProcessCompose, toProcessCompose)
import CI.ProcessCompose.Events (subscribeStates)
import Control.Concurrent.Async (link, wait, withAsync)
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (die, exitWith)
import System.FilePath ((</>))

-- | The runtime artifact paths under @\$PWD\/.ci\/@. Built once at the top
-- of a run so 'runLocal' and 'runStrict' both reference the same
-- convention instead of hand-rolling @runDir \<\/\> "pc.log"@ at each
-- call site. @runRoot@ is the @.ci\/@ directory itself; per-sha log
-- directories (@.ci\/\<sha\>\/@) are derived from it inside 'runStrict'
-- once the SHA is known.
data RunDir = RunDir
  { runRoot :: FilePath,
    worktreePath :: FilePath,
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
      { runRoot = dir,
        worktreePath = dir </> "worktree",
        sock = dir </> "pc.sock",
        pcLog = dir </> "pc.log"
      }

-- | Local mode: live working tree, no observer, no status posts. The
-- process-compose log goes to @.ci\/pc.log@ so even local runs don't leak
-- into @\$TMPDIR@. Process-compose binds @.ci\/pc.sock@ in both modes
-- so the API surface is available for future consumers (e.g. an MCP
-- server).
runLocal :: RunDir -> [String] -> IO ()
runLocal dirs passthrough = do
  pc <- buildProcessCompose LocalRun
  runProcessCompose (UpInvocation dirs.sock dirs.pcLog passthrough) pc >>= exitWith

-- | Strict mode: clean-tree refuse → resolve repo + SHA → snapshot HEAD
-- via @git worktree@ at @.ci\/worktree@ → start process-compose with its
-- API on @.ci\/pc.sock@ → subscribe to state events and post commit
-- statuses concurrently with the pipeline run.
--
-- Per-recipe stdout/stderr is split into @.ci\/\<sha\>\/\<recipe\>.log@
-- (created here before process-compose spawns) so each GitHub commit
-- status can carry a navigable path to the matching log. The SHA-keyed
-- directory keeps history across runs: a green-then-red sequence on the
-- same checkout leaves both runs' logs side-by-side under @.ci\/@.
runStrict :: RunDir -> [String] -> IO ()
runStrict dirs passthrough = do
  dieOnLeft =<< ensureCleanTree
  repo <- dieOnLeft =<< viewRepo
  sha <- dieOnLeft =<< resolveSha
  let logDir = logDirFor dirs.runRoot sha
  createDirectoryIfMissing True logDir
  withSnapshotWorktree dirs.worktreePath $ do
    pc <- buildProcessCompose (StrictRun dirs.worktreePath logDir)
    seedPending repo sha logDir (processNames pc)
    withAsync (subscribeStates dirs.sock (postStatusFor repo sha logDir)) $ \obs -> do
      -- 'link' propagates an observer crash to this thread, so any path
      -- past 'wait' below is a clean WebSocket close (process-compose
      -- shutdown closes the WS on its own).
      link obs
      ec <- runProcessCompose (UpInvocation dirs.sock dirs.pcLog passthrough) pc
      wait obs
      exitWith ec

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
buildProcessCompose :: RunMode -> IO ProcessCompose
buildProcessCompose mode = do
  recipes <- dieOnLeft =<< fetchDump
  root <- dieOnLeft $ findRoot recipes
  reachable <- dieOnLeft $ reachableSubgraph root recipes
  graph <- dieOnLeft $ lowerToRunnerGraph reachable
  let (workingDir, logDir) = case mode of
        LocalRun -> (Nothing, Nothing)
        StrictRun wt ld -> (Just wt, Just ld)
  pure $ toProcessCompose workingDir recipeCommand (mkLogLocation logDir) graph

-- | Per-recipe log file path: @\<logDir\>\/\<recipe\>.log@ when a log
-- directory is configured, 'Nothing' otherwise. The filename convention
-- itself ('CI.CommitStatus.logPathFor') is shared with the status-post
-- side so the on-disk file and the path in the GitHub description
-- cannot drift.
mkLogLocation :: Maybe FilePath -> RecipeName -> Maybe FilePath
mkLogLocation logDir recipe = (`logPathFor` recipe) <$> logDir

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
