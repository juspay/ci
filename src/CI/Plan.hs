{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Adapter between the raw justfile schema ('CI.Justfile') and the runner.
-- The scheduler doesn't need to know what a recipe /does/ — it only needs
-- the dep edges and the parallel/sequential flag, because actual execution
-- delegates to @just --no-deps \<name\>@ (see 'CI.Executor'). So the spec
-- carries only that scheduling-relevant slice.
module CI.Plan
  ( RunSpec (..),
    Plan,
    planFromRoot,
    planFromRecipes,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Graph (ReachError, reachableSubgraph)
import CI.Justfile (Attribute (..), Recipe (..), RecipeName, recipeDeps)
import qualified Data.Map.Strict as Map

-- | What the runner needs to know about a single recipe: how to order its
-- deps. The body is whatever @just --no-deps \<name\>@ executes; we don't
-- mirror it here.
data RunSpec = RunSpec
  { -- | Direct dep names, in declaration order.
    deps :: [RecipeName],
    -- | Whether this recipe's @[parallel]@ attribute is set. When true, the
    -- scheduler runs its 'deps' concurrently; otherwise sequentially.
    parallelDeps :: Bool
  }

-- | The whole plan: every recipe in the reachable subgraph, keyed by name.
type Plan = Map.Map RecipeName RunSpec

-- | One-stop entry point: from a decoded justfile and a root recipe, narrow
-- to the reachable subgraph and translate to a 'Plan'. Hides the
-- reach-then-restrict-then-translate sequence so callers don't reproduce
-- it. Fails with 'ReachError' if @root@ is absent.
planFromRoot :: RecipeName -> Map.Map RecipeName Recipe -> Either ReachError Plan
planFromRoot root recipes = do
  subgraph <- reachableSubgraph root (fmap recipeDeps recipes)
  pure $ planFromRecipes (Map.restrictKeys recipes (G.vertexSet subgraph))

-- | Build a 'Plan' from a recipe map without narrowing. Most callers want
-- 'planFromRoot'; this is the lower-level building block.
planFromRecipes :: Map.Map RecipeName Recipe -> Plan
planFromRecipes = fmap toSpec
  where
    toSpec :: Recipe -> RunSpec
    toSpec r =
      RunSpec
        { deps = recipeDeps r,
          parallelDeps = any isParallel r.attributes
        }
    isParallel Parallel = True
    isParallel _ = False
