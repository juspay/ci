{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Argv parsing + dispatch to 'CI.Pipeline'. Two subcommands:
--
--   * @run [-- ARGS...]@ (default): drive the pipeline; @--@-args pass
--     through to @process-compose up@. Mode is gated on @CI=true@.
--   * @dump-yaml@: print the assembled YAML to stdout.
--
-- @--tui@ is a top-level flag: @ci --tui@ and @ci --tui run@ both
-- work. The flag is only consulted in @run@ mode (it drives
-- process-compose's TUI); other subcommands ignore it silently.
module Main where

import CI.Pipeline (RunMode (..), buildProcessCompose, ensureRunDir, runGraph, runLocal, runStrict)
import Control.Applicative (many, optional, (<|>))
import qualified Data.ByteString as BS
import qualified Data.Yaml as Y
import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    strArgument,
    strOption,
    subparser,
    switch,
    (<**>),
  )
import qualified Options.Applicative as O (command)
import System.Environment (lookupEnv)

-- | Parsed argv: a top-level @--tui@ flag plus the chosen subcommand.
-- @--tui@ lives at the top so @ci --tui@ and @ci --tui run@ both work
-- without nesting the flag inside the @run@ subparser.
data Args = Args {tui :: Bool, cmd :: Command}

-- | The parsed subcommand. 'Run' carries any @-- ...@ passthrough args;
-- 'DumpYaml' has no options of its own; 'Graph' carries the
-- @process-compose graph --format@ choice (ascii/mermaid/json/yaml).
data Command
  = Run [String]
  | DumpYaml
  | Graph (Maybe String)

main :: IO ()
main = do
  Args {tui, cmd} <- execParser parserInfo
  case cmd of
    Run passthrough -> do
      dirs <- ensureRunDir
      strict <- (== Just "true") <$> lookupEnv "CI"
      if strict
        then runStrict dirs tui passthrough
        else runLocal dirs tui passthrough
    DumpYaml -> do
      pc <- buildProcessCompose DumpRun
      BS.putStr (Y.encode pc)
    Graph fmt -> runGraph fmt

parserInfo :: ParserInfo Args
parserInfo =
  info
    (argsParser <**> helper)
    (fullDesc <> progDesc "Drive CI by translating the just recipe graph into process-compose")

-- | Top-level parser: @--tui@ first, then the subcommand. The TUI
-- switch sits here (not inside 'runParser') so callers don't have to
-- remember whether the flag goes before or after the subcommand name.
argsParser :: Parser Args
argsParser =
  Args
    <$> switch (long "tui" <> help "Drive process-compose's TUI instead of its headless logger. Only consulted in run mode.")
    <*> commandParser

commandParser :: Parser Command
commandParser =
  subparser
    ( O.command "run" (info runParser (progDesc "Execute the CI pipeline via process-compose (default). Args after -- are passed through."))
        <> O.command "dump-yaml" (info (pure DumpYaml) (progDesc "Print the process-compose YAML to stdout"))
        <> O.command "graph" (info graphParser (progDesc "Print the process dependency graph (wraps `process-compose graph`)"))
    )
    <|> runParser

runParser :: Parser Command
runParser = Run <$> many (strArgument (metavar "-- ARGS..."))

graphParser :: Parser Command
graphParser =
  Graph
    <$> optional
      ( strOption
          ( long "format"
              <> short 'f'
              <> metavar "FORMAT"
              <> help "Output format passed to process-compose graph: ascii (default), mermaid, json, or yaml."
          )
      )
