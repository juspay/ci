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
import Control.Concurrent.Async (withAsync)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import Data.Time.Clock.POSIX (getPOSIXTime)
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
import System.Directory (getTemporaryDirectory)
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
      strict <- (== Just "true") <$> lookupEnv "CI"
      if strict
        then runStrict extraArgs
        else runLocal extraArgs
    DumpYaml -> do
      pc <- buildProcessCompose Nothing
      BS.putStr (Y.encode pc)

-- | Local mode: live working tree, no observer, no status posts. Behavior
-- is byte-identical to invoking @process-compose up@ on the dumped YAML.
runLocal :: [String] -> IO ()
runLocal extraArgs = do
  pc <- buildProcessCompose Nothing
  runPipeline NoServer extraArgs pc >>= exitWith

-- | Strict mode: clean-tree refuse → resolve coords + SHA → snapshot HEAD
-- via @git worktree@ → start process-compose with its API on a UDS →
-- subscribe to state events and post commit statuses concurrently with the
-- pipeline run.
runStrict :: [String] -> IO ()
runStrict extraArgs = do
  dieOnLeft =<< ensureCleanTree
  coords <- dieOnLeft =<< resolveRepoCoords
  sha <- dieOnLeft =<< resolveSha
  withSnapshotWorktree $ \snap -> do
    sockPath <- pickSocketPath
    pc <- buildProcessCompose (Just snap)
    withAsync (runObserver sockPath [postConsumer coords sha]) $ \_ ->
      runPipeline (UnixSocket sockPath) extraArgs pc >>= exitWith

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

-- | Path for the transient process-compose API UDS. Microsecond timestamp
-- suffix so concurrent runs don't collide.
pickSocketPath :: IO FilePath
pickSocketPath = do
  tmp <- getTemporaryDirectory
  now <- getPOSIXTime
  pure $ tmp </> ("ci-pc-" <> show (round (now * 1e6) :: Integer) <> ".sock")

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
