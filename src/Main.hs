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

import CI.CommitStatus (postConsumer)
import CI.Resolve (ensureCleanTree, resolveRepoCoords, resolveSha)
import CI.Entrypoint (findEntrypoint)
import CI.Graph (lowerToRunnerGraph, reachableSubgraph)
import CI.Justfile (RecipeName, fetchDump, justBin)
import CI.Observer (runObserver)
import CI.ProcessCompose (ProcessCompose, toProcessCompose)
import CI.Runner (ServerMode (..), runPipeline)
import CI.Snapshot (withSnapshotWorktree)
import Control.Applicative (many, (<|>))
import Control.Concurrent.Async (link, waitCatch, withAsync)
import qualified Data.ByteString as BS
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
import System.IO (hPutStrLn, stderr)

data Command
  = Run [String]
  | DumpYaml

-- | The four artifact paths under @\$PWD\/.ci\/@. Built once at the top of
-- 'main' so 'runLocal' and 'runStrict' both reference the same convention
-- instead of hand-rolling @runDir \<\/\> "pc.log"@ at each call site.
data RunDir = RunDir
  { snap :: FilePath,
    sock :: FilePath,
    pcLog :: FilePath
  }

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmd of
    Run extraArgs -> do
      dirs <- ensureRunDir
      strict <- (== Just "true") <$> lookupEnv "CI"
      if strict
        then runStrict dirs extraArgs
        else runLocal dirs extraArgs
    DumpYaml -> do
      pc <- buildProcessCompose Nothing
      BS.putStr (Y.encode pc)

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
runLocal dirs extraArgs = do
  pc <- buildProcessCompose Nothing
  runPipeline NoServer dirs.pcLog extraArgs pc >>= exitWith

-- | Strict mode: clean-tree refuse → resolve coords + SHA → snapshot HEAD
-- via @git worktree@ at @.ci\/snap@ → start process-compose with its API
-- on @.ci\/pc.sock@ → subscribe to state events and post commit statuses
-- concurrently with the pipeline run.
runStrict :: RunDir -> [String] -> IO ()
runStrict dirs extraArgs = do
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
      ec <- runPipeline (UnixSocket dirs.sock) dirs.pcLog extraArgs pc
      obsResult <- waitCatch obs
      case obsResult of
        Right () -> pure ()
        Left e -> hPutStrLn stderr $ "observer: exited with error: " <> show e
      exitWith ec

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
