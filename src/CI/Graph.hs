{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compute the dependency subgraph reachable from a root recipe.
module CI.Graph
  ( -- * Computation
    reachableSubgraph,

    -- * Errors
    ReachError (..),
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import CI.Justfile (RecipeName)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text.Display (Display (..))

-- | Failures from 'reachableSubgraph'.
data ReachError = MissingRecipe RecipeName
  deriving stock (Show)

instance Display ReachError where
  displayBuilder (MissingRecipe r) =
    "recipe " <> displayBuilder r <> " not found in justfile"

-- | The subgraph of the recipe graph reachable from @root@. Input is the
-- adjacency list (recipe name to its direct dep names); anything richer
-- is the caller's concern. Returns 'Left' if @root@ isn't a key.
reachableSubgraph :: RecipeName -> Map.Map RecipeName [RecipeName] -> Either ReachError (G.AdjacencyMap RecipeName)
reachableSubgraph root adj
  -- Reject missing roots up front; G.reachable on an absent vertex
  -- silently returns [root], which would yield a one-vertex graph.
  | Map.notMember root adj = Left $ MissingRecipe root
  | otherwise = Right $ G.induce (`Set.member` keep) recipeGraph
  where
    recipeGraph = G.stars (Map.toList adj)
    keep = Set.fromList $ G.reachable recipeGraph root
