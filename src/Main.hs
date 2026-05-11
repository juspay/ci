{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson (FromJSON (..), eitherDecode, encode, withObject, (.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
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

data Parsed = Parsed {parsedRaw :: A.Object, parsedGraph :: Map.Map Text Recipe}

instance FromJSON Parsed where
  parseJSON = withObject "Dump" $ \o -> Parsed o <$> o .: "recipes"

main :: IO ()
main = do
  let root = "ci" :: Text
  raw <- BL.pack <$> readProcess justBin ["--dump", "--dump-format", "json"] ""
  Parsed top g <- either die pure (eitherDecode raw)
  reach <- either die pure (reachable root g)
  BL.putStrLn (encode (filterRecipes reach top))

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

filterRecipes :: Set.Set Text -> A.Object -> A.Object
filterRecipes keep obj = case KM.lookup "recipes" obj of
  Just (A.Object r) ->
    let r' = KM.filterWithKey (\k _ -> Set.member (K.toText k) keep) r
     in KM.insert "recipes" (A.Object r') obj
  _ -> obj
