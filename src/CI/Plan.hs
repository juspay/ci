{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Adapter between the raw justfile schema ('CI.Justfile') and the runner.
-- Boils each 'Recipe' down to just what the scheduler and executor need —
-- dep names, whether deps fan out, and the body lines to shell out — so the
-- downstream modules never see 'Attribute' or 'Parameter'.
module CI.Plan
  ( DepSpec (..),
    ExecSpec (..),
    RunSpec (..),
    Plan,
    planFromRoot,
    planFromRecipes,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Graph (ReachError, reachableSubgraph)
import CI.Justfile (Attribute (..), Recipe (..), RecipeName, recipeDeps)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Scheduling-side projection of a recipe: how its deps are ordered. The
-- scheduler reads this and nothing else.
data DepSpec = DepSpec
  { -- | Direct dep names, in declaration order.
    deps :: [RecipeName],
    -- | Whether this recipe's @[parallel]@ attribute is set. When true, the
    -- scheduler runs its 'deps' concurrently; otherwise sequentially.
    parallelDeps :: Bool
  }

-- | Execution-side projection: how to actually run the recipe body. The
-- executor reads this and nothing else.
newtype ExecSpec = ExecSpec
  { -- | Body lines to execute under @sh -c@. Empty for pure dep-aggregators
    -- (@ci@, @checks@). Each entry is one line with just's leading @\@@
    -- (silent) marker stripped — we always echo via the sink layer.
    bodyLines :: [Text]
  }

-- | What the runner needs to know about a single recipe — a pair of the two
-- projections above so they evolve independently. Adding a scheduling-only
-- field (e.g. @timeout@) extends 'DepSpec' without touching the executor's
-- signature, and vice versa.
data RunSpec = RunSpec
  { depSpec :: DepSpec,
    execSpec :: ExecSpec
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
        { depSpec =
            DepSpec
              { deps = recipeDeps r,
                parallelDeps = any isParallel r.attributes
              },
          execSpec = ExecSpec {bodyLines = map flattenLine r.body}
        }
    isParallel Parallel = True
    isParallel _ = False
    -- Concat just's per-line fragments and strip the leading silence
    -- marker; interpolation fragments are dropped (no interpolating
    -- recipes today).
    flattenLine = T.dropWhile (== '@') . T.concat
