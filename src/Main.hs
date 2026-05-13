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
import CI.CommitStatus (buildPoster)
import CI.Graph (lowerToRunnerGraph, reachableSubgraph)
import CI.Justfile (RecipeName, fetchDump)
import CI.ProcessCompose (ProcessCompose, toProcessCompose)
import CI.RecipeStep (runStep)
import CI.Runner (runPipeline)
import Control.Applicative (many, (<|>))
import qualified Data.ByteString as BS
import Data.String (fromString)
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
import System.Environment (getExecutablePath)
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
      pc <- buildProcessCompose =<< runStepCommand
      runPipeline extraArgs pc >>= exitWith
    DumpYaml -> do
      pc <- buildProcessCompose =<< runStepCommand
      BS.putStr (Y.encode pc)
    RunStep name -> do
      poster <- dieOnLeft =<< buildPoster name
      runStep poster name >>= exitWith

-- | Produce the per-recipe shell command process-compose runs: @\<self\>
-- run-step \<name\>@. Absolute self-path is baked in because process-compose
-- spawns commands through a shell whose PATH may not contain this binary.
runStepCommand :: IO (RecipeName -> Text)
runStepCommand = do
  self <- T.pack <$> getExecutablePath
  pure $ \name -> self <> " run-step " <> display name

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

buildProcessCompose :: (RecipeName -> Text) -> IO ProcessCompose
buildProcessCompose mkCommand = do
  recipes <- dieOnLeft =<< fetchDump
  root <- dieOnLeft $ findEntrypoint recipes
  reachable <- dieOnLeft $ reachableSubgraph root recipes
  graph <- dieOnLeft $ lowerToRunnerGraph reachable
  pure $ toProcessCompose mkCommand graph

dieOnLeft :: Display e => Either e a -> IO a
dieOnLeft = either (die . T.unpack . display) pure
