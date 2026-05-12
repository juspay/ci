{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compute the dependency adjacency reachable from a root recipe.
module CI.Graph
  ( -- * Computation
    reachableAdjacency,

    -- * Errors
    ReachError (..),
    displayReachError,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import CI.Justfile (Recipe, RecipeName, recipeDeps)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Failures from 'reachableAdjacency'.
data ReachError = MissingRecipe RecipeName
  deriving stock (Show)

-- | Human-readable message for a 'ReachError'.
displayReachError :: ReachError -> Text
displayReachError (MissingRecipe r) = "recipe " <> T.pack (show r) <> " not found in justfile"

-- | Adjacency map (recipe → its direct deps) for every recipe reachable from @root@. Returns 'Left' if @root@ is not a key of the input map.
reachableAdjacency :: RecipeName -> Map.Map RecipeName Recipe -> Either ReachError (Map.Map RecipeName [RecipeName])
reachableAdjacency root g
  -- Reject missing roots up front; G.reachable on an absent vertex
  -- silently returns [root], which would yield a one-key adjacency map.
  | Map.notMember root g = Left (MissingRecipe root)
  | otherwise = Right (recipeDeps <$> Map.restrictKeys g keep)
  where
    recipeGraph = G.stars [(name, recipeDeps r) | (name, r) <- Map.toList g]
    keep = Set.fromList (G.reachable recipeGraph root)
