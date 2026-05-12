{-# LANGUAGE OverloadedStrings #-}

-- | Entry point: print the dependency adjacency reachable from @just ci@.
module Main where

import CI.Graph (reachableAdjacency)
import CI.Justfile (RecipeName, fetchDump)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Exit (die)

main :: IO ()
main = do
  let root = "ci" :: RecipeName
  g <- either die pure =<< fetchDump
  adj <- either die pure (reachableAdjacency root g)
  BL.putStrLn (encodePretty adj)
