{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson (FromJSON (..), eitherDecode, encode, withObject, (.:))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (die)
import System.Process (readProcess)
import System.Which (staticWhich)

justBin :: FilePath
justBin = $(staticWhich "just")

newtype Recipe = Recipe {recipeDeps :: [Text]}

instance FromJSON Recipe where
  parseJSON = withObject "Recipe" $ \o ->
    Recipe <$> (mapM (withObject "Dep" (.: "recipe")) =<< o .: "dependencies")

newtype Dump = Dump {dumpRecipes :: Map.Map Text Recipe}

instance FromJSON Dump where
  parseJSON = withObject "Dump" $ \o -> Dump <$> o .: "recipes"

main :: IO ()
main = do
  let root = "ci" :: Text
  raw <- BL.pack <$> readProcess justBin ["--dump", "--dump-format", "json"] ""
  Dump g <- either die pure (eitherDecode raw)
  reach <- either die pure (reachable root g)
  BL.putStrLn (encode (adjacency reach g))

reachable :: Text -> Map.Map Text Recipe -> Either String (Set.Set Text)
reachable root g
  | Map.notMember root g = Left ("recipe " <> T.unpack root <> " not found in justfile")
  | otherwise = Right (bfs (maybe [] recipeDeps . (`Map.lookup` g)) [root])

bfs :: (Ord a) => (a -> [a]) -> [a] -> Set.Set a
bfs next = go Set.empty
  where
    go seen [] = seen
    go seen (x : xs)
      | Set.member x seen = go seen xs
      | otherwise = go (Set.insert x seen) (next x <> xs)

adjacency :: Set.Set Text -> Map.Map Text Recipe -> Map.Map Text [Text]
adjacency keep g = recipeDeps <$> Map.restrictKeys g keep
