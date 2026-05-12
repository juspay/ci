{-# LANGUAGE OverloadedStrings #-}

-- | Entry point: emit a @process-compose@ YAML config for the @just@ recipe
-- subgraph reachable from the @[metadata(\"entrypoint\")]@ recipe.
module Main where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Graph (reachableSubgraph)
import CI.Justfile (fetchDump, findEntrypoint)
import CI.ProcessCompose (toProcessCompose)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Display (display)
import qualified Data.Yaml as Y
import System.Exit (die)

main :: IO ()
main = do
  recipes <- either (die . T.unpack . display) pure =<< fetchDump
  root <- either (die . T.unpack . display) pure $ findEntrypoint recipes
  subgraph <- either (die . T.unpack . display) pure $ reachableSubgraph root recipes
  let reachable = Map.restrictKeys recipes (G.vertexSet subgraph)
  pc <- either (die . T.unpack . display) pure $ toProcessCompose reachable
  BS.putStr $ Y.encode pc
