{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import CI.Graph (reachable)
import CI.Justfile (fetchDump, recipeDeps)
import System.Exit (die)

main :: IO ()
main = do
  let root = "ci" :: Text
  g <- fetchDump
  case Map.lookup root g of
    Nothing -> die ("recipe " <> T.unpack root <> " not found in justfile")
    Just _ -> do
      let reach = reachable (maybe [] recipeDeps . (`Map.lookup` g)) root
          adj = recipeDeps <$> Map.restrictKeys g reach
      BL.putStrLn (encodePretty adj)
