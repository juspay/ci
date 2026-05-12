{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Recipe-graph computations: reachability from a root, and a runnable
-- execution graph that bakes just's sequential-vs-parallel dep ordering into
-- explicit edges.
module CI.Graph
  ( -- * Reachability
    reachableSubgraph,
    ReachError (..),

    -- * Execution graph
    buildExecutionGraph,
    OrderingConflict (..),
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import CI.Justfile (Attribute (..), Dep (..), Recipe (..), RecipeName)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)

-- | Failures from 'reachableSubgraph'.
data ReachError = MissingRecipe RecipeName
  deriving stock (Show)

instance Display ReachError where
  displayBuilder (MissingRecipe r) =
    "recipe " <> displayBuilder r <> " not found in justfile"

-- | The recipes reachable from @root@ along their declared dependencies. Returns 'Left' if @root@ isn't a key of the input map.
reachableSubgraph :: RecipeName -> Map.Map RecipeName Recipe -> Either ReachError (Map.Map RecipeName Recipe)
reachableSubgraph root g
  -- Reject missing roots up front; G.reachable on an absent vertex
  -- silently returns [root], which would yield a one-vertex graph.
  | Map.notMember root g = Left $ MissingRecipe root
  | otherwise = Right $ Map.restrictKeys g keep
  where
    recipeGraph =
      G.stars
        [ (name, [d.recipe | d <- r.dependencies])
          | (name, r) <- Map.toList g
        ]
    keep = Set.fromList $ G.reachable recipeGraph root

-- | The recipes cannot be linearized: their dependencies form a cycle.
-- Carries the cycling recipes in the order @topSort@ returned them.
newtype OrderingConflict = OrderingConflict {cycleNodes :: NE.NonEmpty RecipeName}
  deriving stock (Show)

instance Display OrderingConflict where
  displayBuilder (OrderingConflict c) =
    "recipe dependencies form a cycle: "
      <> displayBuilder (T.intercalate " -> " (display <$> NE.toList c))

-- | Build the directed execution graph for a recipe map.
--
-- Per recipe @R@ with deps @[d1..dn]@:
--
--   * If @R@ has @[parallel]@ (or has ≤ 1 dep): @R@ depends on every dep
--     directly; siblings are unconstrained, so the runner can fan out.
--
--   * Otherwise (sequential semantics, which is just's default): @R@ depends
--     only on the last dep; each later dep is augmented to depend on its
--     predecessor, forming a chain @d1 ← d2 ← … ← dn ← R@. Augmenting
--     /shared/ dep nodes is the only way to express caller-side ordering when
--     the downstream runner only understands per-node @depends_on@; the
--     cycle check guards against contradictory chains from different callers.
buildExecutionGraph ::
  Map.Map RecipeName Recipe ->
  Either OrderingConflict (G.AdjacencyMap RecipeName)
buildExecutionGraph recipes =
  case G.topSort g of
    Left c -> Left (OrderingConflict c)
    Right _ -> Right g
  where
    g = G.overlay (G.vertices (Map.keys recipes)) (G.edges (concatMap callerEdges (Map.toList recipes)))

callerEdges :: (RecipeName, Recipe) -> [(RecipeName, RecipeName)]
callerEdges (name, r) =
  case map (.recipe) r.dependencies of
    [] -> []
    deps
      | isParallel r.attributes || length deps == 1 ->
          [(name, d) | d <- deps]
      | otherwise ->
          let chain = zip (drop 1 deps) deps
           in (name, last deps) : chain

isParallel :: [Attribute] -> Bool
isParallel = any (\case Parallel -> True; _ -> False)
