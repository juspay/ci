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
import CI.Justfile (Recipe, RecipeName, fetchDump, recipeDeps)
import CI.Plan (planFromRoot)
import CI.Scheduler (runPlan)
import qualified CI.Sinks as Sinks
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Display (display)
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
  recipes <- fetchOrDie
  subgraph <- subgraphOrDie root recipes
  BL.putStrLn $ encodePretty $ G.adjacencyMap subgraph

runRecipe :: RecipeName -> IO ()
runRecipe root = do
  recipes <- fetchOrDie
  plan <- either (die . T.unpack . display) pure (planFromRoot root recipes)
  liveTail <- Sinks.newLiveTail stderr
  let sinks =
        Sinks.Sinks
          { Sinks.logDir = Just ".ci-logs",
            Sinks.liveTail = Just liveTail
          }
  code <- runPlan (exec sinks) plan root
  exitWith code

fetchOrDie :: IO (Map.Map RecipeName Recipe)
fetchOrDie = either (die . T.unpack . display) pure =<< fetchDump

subgraphOrDie ::
  RecipeName ->
  Map.Map RecipeName Recipe ->
  IO (G.AdjacencyMap RecipeName)
subgraphOrDie root recipes =
  either (die . T.unpack . display) pure (reachableSubgraph root (fmap recipeDeps recipes))
