{-# LANGUAGE OverloadedStrings #-}

-- | Argv parsing + dispatch to 'CI.Pipeline'. Two subcommands:
--
--   * @run [-- ARGS...]@ (default): drive the pipeline; @--@-args pass
--     through to @process-compose up@. Mode is gated on @CI=true@.
--   * @dump-yaml@: print the assembled YAML to stdout.
module Main where

import CI.Pipeline (buildProcessCompose, ensureRunDir, runLocal, runStrict)
import Control.Applicative (many, (<|>))
import qualified Data.ByteString as BS
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
import System.Environment (lookupEnv)

-- | Parsed argv. 'Run' carries the passthrough args after @--@.
data Command
  = Run [String]
  | DumpYaml

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmd of
    Run passthrough -> do
      dirs <- ensureRunDir
      strict <- (== Just "true") <$> lookupEnv "CI"
      if strict
        then runStrict dirs passthrough
        else runLocal dirs passthrough
    DumpYaml -> do
      pc <- buildProcessCompose Nothing
      BS.putStr (Y.encode pc)

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
