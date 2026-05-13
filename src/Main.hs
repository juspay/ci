{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Entry point. Three subcommands share an assembled 'ProcessCompose':
--
--   * @run [-- ARGS...]@ (the default): hand the config to @process-compose@
--     and execute the pipeline. Anything after @--@ is forwarded verbatim
--     to @process-compose up@.
--   * @dump-yaml@: emit the @process-compose@ YAML on stdout.
--   * @run-step \<recipe\>@: lifecycle wrapper invoked by process-compose for
--     each vertex. Posts GitHub commit-status transitions when @CI=true@.
module Main where

import CI.Entrypoint (findEntrypoint)
import CI.GitHubStatus (Context (..), postStatus, resolveRepoCoords, resolveSha)
import CI.Graph (lowerToRunnerGraph, reachableSubgraph)
import CI.Justfile (RecipeName, fetchDump)
import CI.ProcessCompose (Process (..), ProcessCompose (..), toProcessCompose)
import CI.RecipeStep (CommitStatus, runStep)
import CI.Runner (runPipeline)
import Control.Applicative (many, (<|>))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.String (fromString)
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
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (die, exitWith)

data Command
  = Run [String]
  | DumpYaml
  | RunStep RecipeName

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmd of
    Run extraArgs -> do
      pc <- buildWrappedProcessCompose
      runPipeline extraArgs pc >>= exitWith
    DumpYaml -> do
      pc <- buildWrappedProcessCompose
      BS.putStr (Y.encode pc)
    RunStep name -> do
      poster <- buildPoster name
      runStep poster name >>= exitWith

parserInfo :: ParserInfo Command
parserInfo =
  info
    (commandParser <**> helper)
    (fullDesc <> progDesc "Drive CI by translating the just recipe graph into process-compose")

-- | Bare invocation defaults to @Run []@ via the @\<|\> pure (Run [])@ fallthrough.
commandParser :: Parser Command
commandParser =
  subparser
    ( O.command "run" (info (Run <$> many (strArgument (metavar "-- ARGS..."))) (progDesc "Execute the CI pipeline via process-compose (default). Args after -- are passed through."))
        <> O.command "dump-yaml" (info (pure DumpYaml) (progDesc "Print the process-compose YAML to stdout"))
        <> O.command "run-step" (info (RunStep . fromString <$> strArgument (metavar "RECIPE")) (progDesc "Run a single recipe with lifecycle reporting (invoked by process-compose)"))
    )
    <|> pure (Run [])

buildWrappedProcessCompose :: IO ProcessCompose
buildWrappedProcessCompose = do
  self <- getExecutablePath
  wrapForCI self <$> buildProcessCompose

buildProcessCompose :: IO ProcessCompose
buildProcessCompose = do
  recipes <- dieOnLeft =<< fetchDump
  root <- dieOnLeft $ findEntrypoint recipes
  reachable <- dieOnLeft $ reachableSubgraph root recipes
  graph <- dieOnLeft $ lowerToRunnerGraph reachable
  pure $ toProcessCompose graph

-- | Rewrite every process's @command@ to invoke @\<self\> run-step \<name\>@
-- so the lifecycle wrapper runs for every vertex. Absolute self-path is
-- baked in because process-compose spawns commands through a shell whose
-- PATH may not contain this binary.
wrapForCI :: FilePath -> ProcessCompose -> ProcessCompose
wrapForCI self (ProcessCompose ps) =
  ProcessCompose (Map.mapWithKey rewrap ps)
  where
    rewrap name p = p {command = T.pack self <> " run-step " <> display name}

-- | Construct the lifecycle poster for a given recipe. When @CI=true@,
-- resolve repo/SHA once and return a 'postStatus' closure under the
-- @ci/\<recipe\>@ context; otherwise return a no-op so dev runs stay silent.
-- The env-driven branch is the only feature gate in the pipeline; the YAML
-- always emits @run-step@.
buildPoster :: RecipeName -> IO (CommitStatus -> IO ())
buildPoster name = do
  enabled <- (== Just "true") <$> lookupEnv "CI"
  if not enabled
    then pure (\_ -> pure ())
    else do
      coords <- resolveRepoCoords
      sha <- resolveSha
      let ctx = Context ("ci/" <> display name)
      pure (postStatus coords sha ctx)

dieOnLeft :: Display e => Either e a -> IO a
dieOnLeft = either (die . T.unpack . display) pure
