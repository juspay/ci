{-# LANGUAGE OverloadedStrings #-}

-- | Entry point: emit a @process-compose@ YAML config for the @just@ recipe
-- subgraph reachable from the @[metadata(\"entrypoint\")]@ recipe.
module Main where

import CI.Graph (buildExecutionGraph, reachableSubgraph)
import CI.Justfile (fetchDump, findEntrypoint)
import CI.ProcessCompose (toProcessCompose)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import qualified Data.Yaml as Y
import System.Exit (die)

main :: IO ()
main = do
  recipes <- dieOnLeft =<< fetchDump
  root <- dieOnLeft $ findEntrypoint recipes
  reachable <- dieOnLeft $ reachableSubgraph root recipes
  graph <- dieOnLeft $ buildExecutionGraph reachable
  BS.putStr $ Y.encode $ toProcessCompose graph

-- | Render an 'Either' error via 'Display' and 'die' with its text.
dieOnLeft :: Display e => Either e a -> IO a
dieOnLeft = either (die . T.unpack . display) pure
