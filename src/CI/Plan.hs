{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Adapter between the raw justfile schema ('CI.Justfile') and the runner.
-- Boils each 'Recipe' down to just what the scheduler and executor need —
-- dep names, whether deps fan out, and the body lines to shell out — so the
-- downstream modules never see 'Attribute' or 'Parameter'.
module CI.Plan
  ( RunSpec (..),
    Plan,
    planFromRecipes,
  )
where

import CI.Justfile (Attribute (..), Dep (..), Recipe (..), RecipeName)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | What the runner needs to know about a single recipe.
data RunSpec = RunSpec
  { -- | Direct dep names, in declaration order.
    deps :: [RecipeName],
    -- | Whether this recipe's @[parallel]@ attribute is set. When true, the
    -- scheduler runs its 'deps' concurrently; otherwise sequentially.
    parallelDeps :: Bool,
    -- | Body lines to execute under @sh -c@. Empty for pure dep-aggregators
    -- (@ci@, @checks@). Each entry is one line with just's leading @\@@
    -- (silent) marker stripped — we always echo via the sink layer.
    bodyLines :: [Text]
  }

-- | The whole plan: every recipe in the reachable subgraph, keyed by name.
type Plan = Map.Map RecipeName RunSpec

-- | Build a 'Plan' from the decoded justfile. Caller has already narrowed to
-- the reachable subgraph if desired.
planFromRecipes :: Map.Map RecipeName Recipe -> Plan
planFromRecipes = fmap toSpec
  where
    toSpec :: Recipe -> RunSpec
    toSpec r =
      RunSpec
        { deps = [d.recipe | d <- r.dependencies],
          parallelDeps = any isParallel r.attributes,
          bodyLines = map flattenLine r.body
        }
    isParallel Parallel = True
    isParallel _ = False
    -- just emits each body line as a list of fragments (literals plus, in
    -- the general case, interpolation chunks). For now we concat the
    -- fragments and strip the leading silence marker if present; recipes
    -- with @{{var}}@ interpolation in their bodies will lose the
    -- interpolation, which is fine for the current justfile (no
    -- interpolations) and a known V1 limitation.
    flattenLine = T.dropWhile (== '@') . T.concat
