{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compute the dependency subgraph reachable from a root recipe.
module CI.Graph
  ( -- * Computation
    reachableSubgraph,

    -- * Errors
    ReachError (..),
    displayReachError,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import CI.Justfile (Dep (..), Recipe (..), RecipeName)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Failures from 'reachableSubgraph'.
data ReachError = MissingRecipe RecipeName
  deriving stock (Show)

-- | Human-readable message for a 'ReachError'.
displayReachError :: ReachError -> Text
displayReachError (MissingRecipe r) = "recipe " <> T.pack (show r) <> " not found in justfile"

-- | The subgraph of the recipe graph reachable from @root@. Returns 'Left' if @root@ isn't a key of the input map.
reachableSubgraph :: RecipeName -> Map.Map RecipeName Recipe -> Either ReachError (G.AdjacencyMap RecipeName)
reachableSubgraph root g
  -- Reject missing roots up front; G.reachable on an absent vertex
  -- silently returns [root], which would yield a one-vertex graph.
  | Map.notMember root g = Left (MissingRecipe root)
  | otherwise = Right (G.induce (`Set.member` keep) recipeGraph)
  where
    recipeGraph =
      G.stars
        [ (name, [d.recipe | d <- r.dependencies])
          | (name, r) <- Map.toList g
        ]
    keep = Set.fromList (G.reachable recipeGraph root)
