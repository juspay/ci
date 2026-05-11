{-# LANGUAGE OverloadedStrings #-}

-- | Entry point: print the dependency adjacency reachable from @just ci@.
module Main where

import CI.Graph (reachableAdjacency)
import CI.Justfile (fetchDump)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import System.Exit (die)

main :: IO ()
main = do
  let root = "ci" :: Text
  g <- fetchDump
  case reachableAdjacency root g of
    Left e -> die e
    Right adj -> BL.putStrLn (encodePretty adj)
