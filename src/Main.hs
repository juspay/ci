{-# LANGUAGE OverloadedStrings #-}

-- | Entry point. Two subcommands consume the same assembled
-- 'ProcessCompose' value:
--
--   * @run@ (the default): hand the config to @process-compose@ and execute
--     the pipeline.
--   * @dump-yaml@: emit the @process-compose@ YAML on stdout.
module Main where

import CI.Entrypoint (findEntrypoint)
import CI.Graph (lowerToRunnerGraph, reachableSubgraph)
import CI.Justfile (fetchDump)
import CI.ProcessCompose (ProcessCompose, toProcessCompose)
import CI.Runner (runPipeline)
import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import qualified Data.Yaml as Y
import Options.Applicative
  ( Parser,
    ParserInfo,
    command,
    execParser,
    fullDesc,
    helper,
    info,
    progDesc,
    subparser,
    (<**>),
  )
import System.Exit (die, exitWith)

data Command = Run | DumpYaml

main :: IO ()
main = do
  cmd <- execParser parserInfo
  pc <- buildProcessCompose
  case cmd of
    Run -> runPipeline pc >>= exitWith
    DumpYaml -> BS.putStr (Y.encode pc)

parserInfo :: ParserInfo Command
parserInfo =
  info
    (commandParser <**> helper)
    (fullDesc <> progDesc "Drive CI by translating the just recipe graph into process-compose")

-- | Bare invocation defaults to 'Run' via the @\<|\> pure Run@ fallthrough.
commandParser :: Parser Command
commandParser =
  subparser
    ( command "run" (info (pure Run) (progDesc "Execute the CI pipeline via process-compose (default)"))
        <> command "dump-yaml" (info (pure DumpYaml) (progDesc "Print the process-compose YAML to stdout"))
    )
    <|> pure Run

-- | Assemble the 'ProcessCompose' config shared by both subcommands.
buildProcessCompose :: IO ProcessCompose
buildProcessCompose = do
  recipes <- dieOnLeft =<< fetchDump
  root <- dieOnLeft $ findEntrypoint recipes
  reachable <- dieOnLeft $ reachableSubgraph root recipes
  graph <- dieOnLeft $ lowerToRunnerGraph reachable
  pure $ toProcessCompose graph

-- | Render an 'Either' error via 'Display' and 'die' with its text.
dieOnLeft :: Display e => Either e a -> IO a
dieOnLeft = either (die . T.unpack . display) pure
