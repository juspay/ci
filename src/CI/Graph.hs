{-# LANGUAGE OverloadedStrings #-}

-- | Compute the dependency adjacency reachable from a root recipe.
module CI.Graph (reachableAdjacency) where

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import CI.Justfile (Recipe, recipeDeps)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

reachableAdjacency :: Text -> Map.Map Text Recipe -> Either String (Map.Map Text [Text])
reachableAdjacency root g
  | Map.notMember root g = Left ("recipe " <> T.unpack root <> " not found in justfile")
  | otherwise = Right (recipeDeps <$> Map.restrictKeys g keep)
  where
    am = G.stars [(name, recipeDeps r) | (name, r) <- Map.toList g]
    keep = Set.fromList (G.reachable am root)
