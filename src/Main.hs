{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Entry point. Two subcommands share an assembled 'ProcessCompose':
--
--   * @run [-- ARGS...]@ (the default): drive the pipeline. Anything after
--     @--@ is forwarded verbatim to @process-compose up@.
--   * @dump-yaml@: emit the @process-compose@ YAML on stdout.
--
-- Two run modes, gated on @CI=true@:
--
--   * Local (@CI@ unset): live working tree, no status posting, no observer.
--   * Strict (@CI=true@): refuse if the tree is dirty; snapshot HEAD to a
--     transient @git worktree@; start process-compose with its API on a
--     UDS; concurrently subscribe to its state-event stream from
--     'CI.Observer' and post a GitHub commit status per transition.
module Main where

import CI.CommitStatus
  ( CommitStatus (..),
    RepoCoords,
    Sha,
    ensureCleanTree,
    mkContext,
    postStatus,
    resolveRepoCoords,
    resolveSha,
  )
import CI.Entrypoint (findEntrypoint)
import CI.Graph (lowerToRunnerGraph, reachableSubgraph)
import CI.Justfile (RecipeName, fetchDump, justBin)
import CI.Observer (ProcessState (..), ProcessStateEvent (..), runObserver)
import CI.ProcessCompose (ProcessCompose, toProcessCompose)
import CI.Runner (ServerMode (..), runPipeline)
import CI.Snapshot (withSnapshotWorktree)
import Control.Applicative (many, (<|>))
import Control.Concurrent.Async (link, waitCatch, withAsync)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import qualified Data.Yaml as Y
import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    fullDesc,
    helper,
    info,
    metavar,
    progDesc,
    strArgument,
    subparser,
    (<**>),
  )
import qualified Options.Applicative as O (command)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (die, exitWith)
import System.FilePath ((</>))

data Command
  = Run [String]
  | DumpYaml

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmd of
    Run extraArgs -> do
      runDir <- ensureRunDir
      strict <- (== Just "true") <$> lookupEnv "CI"
      if strict
        then runStrict runDir extraArgs
        else runLocal runDir extraArgs
    DumpYaml -> do
      pc <- buildProcessCompose Nothing
      BS.putStr (Y.encode pc)

-- | Per-repo runtime-artifact directory. Everything we write — the
-- process-compose UDS, its log file, the @git worktree@ snapshot — lives
-- here so the user gitignores @\/.ci\/@ once and forgets about it.
ensureRunDir :: IO FilePath
ensureRunDir = do
  cwd <- getCurrentDirectory
  let dir = cwd </> ".ci"
  createDirectoryIfMissing True dir
  pure dir

-- | Local mode: live working tree, no observer, no status posts. The
-- process-compose log is redirected to @.ci\/pc.log@ so even local runs
-- don't leak into @\$TMPDIR@.
runLocal :: FilePath -> [String] -> IO ()
runLocal runDir extraArgs = do
  pc <- buildProcessCompose Nothing
  runPipeline NoServer (runDir </> "pc.log") extraArgs pc >>= exitWith

-- | Strict mode: clean-tree refuse → resolve coords + SHA → snapshot HEAD
-- via @git worktree@ at @.ci\/snap@ → start process-compose with its API
-- on @.ci\/pc.sock@ → subscribe to state events and post commit statuses
-- concurrently with the pipeline run.
runStrict :: FilePath -> [String] -> IO ()
runStrict runDir extraArgs = do
  dieOnLeft =<< ensureCleanTree
  coords <- dieOnLeft =<< resolveRepoCoords
  sha <- dieOnLeft =<< resolveSha
  let snapPath = runDir </> "snap"
      sockPath = runDir </> "pc.sock"
      logPath = runDir </> "pc.log"
  withSnapshotWorktree snapPath $ do
    pc <- buildProcessCompose (Just snapPath)
    withAsync (runObserver sockPath [postConsumer coords sha]) $ \obs -> do
      link obs
      ec <- runPipeline (UnixSocket sockPath) logPath extraArgs pc
      -- Wait for the observer to drain remaining events (the WS closes when
      -- process-compose exits, so this is bounded by the close handshake).
      _ <- waitCatch obs
      exitWith ec

-- | Translate a single 'ProcessStateEvent' into at most one 'postStatus'
-- call, under the @ci/\<recipe\>@ context. Non-terminal states without a
-- 'CommitStatus' analogue (Pending, Launching, …) drop on the floor.
postConsumer :: RepoCoords -> Sha -> ProcessStateEvent -> IO ()
postConsumer coords sha (ProcessStateEvent _ ps) =
  for_ (psToCommitStatus ps) (postStatus coords sha (mkContext (name ps)))

psToCommitStatus :: ProcessState -> Maybe CommitStatus
psToCommitStatus ps = case (status ps, exit_code ps) of
  ("Running", _) -> Just Pending
  ("Completed", 0) -> Just Success
  ("Completed", _) -> Just Failure
  ("Skipped", _) -> Just Error
  ("Error", _) -> Just Error
  _ -> Nothing

parserInfo :: ParserInfo Command
parserInfo =
  info
    (commandParser <**> helper)
    (fullDesc <> progDesc "Drive CI by translating the just recipe graph into process-compose")

commandParser :: Parser Command
commandParser =
  subparser
    ( O.command "run" (info (Run <$> many (strArgument (metavar "-- ARGS..."))) (progDesc "Execute the CI pipeline via process-compose (default). Args after -- are passed through."))
        <> O.command "dump-yaml" (info (pure DumpYaml) (progDesc "Print the process-compose YAML to stdout"))
    )
    <|> pure (Run [])

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

dieOnLeft :: Display e => Either e a -> IO a
dieOnLeft = either (die . T.unpack . display) pure
