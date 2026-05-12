{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Entry point. Two modes:
--
--   * No args (or @--graph@): print the dependency adjacency reachable from
--     @just ci@ as pretty JSON. Preserves the original behaviour (see #2).
--
--   * @run [<recipe>]@: build the plan and run that recipe (default: @ci@)
--     under our scheduler, with stdout+stderr streamed to per-recipe log
--     files under @./.ci-logs@ and prefixed live output on stderr.
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
import System.Environment (getArgs)
import System.Exit (die, exitWith)
import System.IO (stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printGraph "ci"
    ["--graph"] -> printGraph "ci"
    ["run"] -> runRecipe "ci"
    ["run", name] -> runRecipe (fromString name)
    _ -> die "usage: ci [--graph] | ci run [<recipe>]"

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
