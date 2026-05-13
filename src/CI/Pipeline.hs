{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Orchestration for the two run modes (local / strict), the runtime
-- artifact layout under @\$PWD\/.ci\/@, and the just-graph-to-YAML build.
-- "Main" is the dispatch layer; everything mode-specific or orchestration-
-- shaped lives here.
module CI.Pipeline
  ( RunDir (..),
    ensureRunDir,
    runLocal,
    runStrict,
    buildProcessCompose,
  )
where

import CI.CommitStatus (postConsumer)
import CI.Entrypoint (findEntrypoint)
import CI.Gh (resolveRepoCoords)
import CI.Git (ensureCleanTree, resolveSha, withSnapshotWorktree)
import CI.Graph (lowerToRunnerGraph, reachableSubgraph)
import CI.Justfile (RecipeName, fetchDump, justBin)
import CI.Observer (runObserver)
import CI.ProcessCompose (ProcessCompose, ServerMode (..), UpInvocation (..), toProcessCompose)
import CI.Runner (runPipeline)
import Control.Concurrent.Async (link, waitCatch, withAsync)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (die, exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

-- | The three artifact paths under @\$PWD\/.ci\/@. Built once at the top of
-- a run so 'runLocal' and 'runStrict' both reference the same convention
-- instead of hand-rolling @runDir \<\/\> "pc.log"@ at each call site.
data RunDir = RunDir
  { snap :: FilePath,
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
  pure RunDir {snap = dir </> "snap", sock = dir </> "pc.sock", pcLog = dir </> "pc.log"}

-- | Local mode: live working tree, no observer, no status posts. The
-- process-compose log goes to @.ci\/pc.log@ so even local runs don't leak
-- into @\$TMPDIR@.
runLocal :: RunDir -> [String] -> IO ()
runLocal dirs extra = do
  pc <- buildProcessCompose Nothing
  runPipeline (UpInvocation NoServer dirs.pcLog extra) pc >>= exitWith

-- | Strict mode: clean-tree refuse → resolve coords + SHA → snapshot HEAD
-- via @git worktree@ at @.ci\/snap@ → start process-compose with its API
-- on @.ci\/pc.sock@ → subscribe to state events and post commit statuses
-- concurrently with the pipeline run.
runStrict :: RunDir -> [String] -> IO ()
runStrict dirs extra = do
  dieOnLeft =<< ensureCleanTree
  coords <- dieOnLeft =<< resolveRepoCoords
  sha <- dieOnLeft =<< resolveSha
  withSnapshotWorktree dirs.snap $ do
    pc <- buildProcessCompose (Just dirs.snap)
    withAsync (runObserver dirs.sock [postConsumer coords sha]) $ \obs -> do
      -- 'link' propagates an observer crash to this thread (so an
      -- observer-side exception aborts the pipeline rather than silently
      -- proceeding with no status posts). 'waitCatch' after the pipeline
      -- finishes lets the observer drain queued events — process-compose's
      -- WS closes on its own shutdown, so this is bounded by the close
      -- handshake.
      link obs
      ec <- runPipeline (UpInvocation (UnixSocket dirs.sock) dirs.pcLog extra) pc
      obsResult <- waitCatch obs
      case obsResult of
        Right () -> pure ()
        Left e -> hPutStrLn stderr $ "observer: exited with error: " <> show e
      exitWith ec

-- | Walk @just --dump@ → entrypoint → reachable subgraph → topologically
-- lowered DAG → 'ProcessCompose' YAML. The @workingDir@ argument is set in
-- strict mode (every recipe @chdir@s into the worktree snapshot); 'Nothing'
-- in local mode and for @dump-yaml@.
buildProcessCompose :: Maybe FilePath -> IO ProcessCompose
buildProcessCompose workingDir = do
  recipes <- dieOnLeft =<< fetchDump
  root <- dieOnLeft $ findEntrypoint recipes
  reachable <- dieOnLeft $ reachableSubgraph root recipes
  graph <- dieOnLeft $ lowerToRunnerGraph reachable
  pure $ toProcessCompose workingDir recipeCommand graph

-- | Per-vertex shell command: @\<absolute-just-path\> --no-deps \<recipe\>@.
-- Absolute path baked in so process-compose's spawned shell finds @just@
-- regardless of PATH.
recipeCommand :: RecipeName -> Text
recipeCommand n = T.pack justBin <> " --no-deps " <> display n

-- | Die at the boundary with the structured error's Display rendering.
dieOnLeft :: Display e => Either e a -> IO a
dieOnLeft = either (die . T.unpack . display) pure
