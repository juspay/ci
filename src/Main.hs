{-# LANGUAGE OverloadedStrings #-}

-- | Entry point. Subcommands:
--
--   * @ci graph@ — print the dependency adjacency reachable from a root
--     recipe (default @ci@) as pretty JSON. Preserves #2's behaviour.
--
--   * @ci run [<recipe>]@ — build the plan and run that recipe (default
--     @ci@) under our scheduler, with stdout+stderr streamed to per-recipe
--     log files under @./.ci-logs@ and prefixed live output on stderr.
module Main where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Executor (exec)
import CI.Graph (reachableSubgraph)
import CI.Justfile (RecipeName, fetchDump, recipeDeps)
import CI.Plan (planFromRoot)
import CI.Scheduler (runPlan)
import CI.Sinks (Sinks (..), newLiveTail)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import Options.Applicative
  ( Parser,
    argument,
    command,
    execParser,
    fullDesc,
    help,
    helper,
    hsubparser,
    info,
    metavar,
    progDesc,
    showDefault,
    str,
    value,
    (<**>),
  )
import System.Exit (die, exitWith)
import System.IO (stderr)

-- | The two top-level invocations the binary supports.
data Command
  = Graph RecipeName
  | Run RecipeName

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (commandP <**> helper)
        ( fullDesc
            <> progDesc "Inspect or run justfile recipes"
        )

run :: Command -> IO ()
run (Graph root) = printGraph root
run (Run root) = runRecipe root

commandP :: Parser Command
commandP =
  hsubparser
    ( command "graph" (info (Graph <$> rootArg) (progDesc "Print the recipe dep graph as JSON"))
        <> command "run" (info (Run <$> rootArg) (progDesc "Run a recipe and its deps"))
    )

-- | Positional recipe argument shared by both subcommands. Defaults to
-- @"ci"@ when omitted.
rootArg :: Parser RecipeName
rootArg =
  argument
    (fromString <$> str)
    ( metavar "RECIPE"
        <> value "ci"
        <> showDefault
        <> help "Root recipe name"
    )

printGraph :: RecipeName -> IO ()
printGraph root = do
  recipes <- dieOnLeft =<< fetchDump
  subgraph <- dieOnLeft (reachableSubgraph root (fmap recipeDeps recipes))
  BL.putStrLn $ encodePretty $ G.adjacencyMap subgraph

runRecipe :: RecipeName -> IO ()
runRecipe root = do
  recipes <- dieOnLeft =<< fetchDump
  plan <- dieOnLeft (planFromRoot root recipes)
  tail' <- newLiveTail stderr
  let sinks = Sinks {logDir = Just ".ci-logs", liveTail = Just tail'}
  code <- runPlan (exec sinks) plan root
  exitWith code

-- | Boundary helper: turn a 'Left' error into 'die' with its 'Display'
-- rendering. Used in 'main' to surface 'FetchError', 'ReachError', and
-- 'PlanError' as terminating CLI output without spreading the formatting
-- recipe across each call site.
dieOnLeft :: (Display e) => Either e a -> IO a
dieOnLeft = either (die . T.unpack . display) pure
