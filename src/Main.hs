{-# LANGUAGE OverloadedStrings #-}

-- | Entry point: print the dependency adjacency reachable from @just ci@.
module Main where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Graph (reachableSubgraph)
import CI.Justfile (RecipeName, fetchDump)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Text.Display (display)
import System.Exit (die)

main :: IO ()
main = do
  let root = "ci" :: RecipeName
  g <- either (die . T.unpack . display) pure =<< fetchDump
  subgraph <- either (die . T.unpack . display) pure $ reachableSubgraph root g
  BL.putStrLn $ encodePretty $ G.adjacencyMap subgraph
