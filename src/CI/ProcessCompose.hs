{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Translate a reachable @just@ recipe graph into a @process-compose@ config.
module CI.ProcessCompose
  ( -- * Schema
    ProcessCompose (..),
    Process (..),
    Condition (..),
    Availability (..),
    RestartPolicy (..),

    -- * Translation
    OrderingConflict (..),
    toProcessCompose,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as GA
import CI.Justfile (Attribute (..), Dep (..), Recipe (..), RecipeName)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)

-- | Top-level @process-compose.yaml@: a map from process name to spec.
newtype ProcessCompose = ProcessCompose {processes :: Map.Map RecipeName Process}

instance ToJSON ProcessCompose where
  toJSON pc = object ["processes" .= pc.processes]

-- | One @processes.<name>@ entry. Field names match @process-compose@'s YAML keys.
data Process = Process
  { command :: Text,
    depends_on :: Map.Map RecipeName Condition,
    availability :: Availability
  }

instance ToJSON Process where
  toJSON p =
    object
      [ "command" .= p.command,
        "depends_on" .= Map.map (\c -> object ["condition" .= c]) p.depends_on,
        "availability" .= p.availability
      ]

-- | The dep-edge condition we emit. Singleton: every dep is "must have exited 0".
data Condition = ProcessCompletedSuccessfully

instance ToJSON Condition where
  toJSON ProcessCompletedSuccessfully = toJSON ("process_completed_successfully" :: Text)

-- | Per-process failure policy. Both knobs are set explicitly: 'ExitOnFailure'
-- propagates a non-zero exit upward; 'exit_on_skipped' also tears down when a
-- dep is skipped (which would otherwise leave the pipeline hanging with no
-- failure signal at the parent).
data Availability = Availability
  { restart :: RestartPolicy,
    exit_on_skipped :: Bool
  }

instance ToJSON Availability where
  toJSON a =
    object
      [ "restart" .= a.restart,
        "exit_on_skipped" .= a.exit_on_skipped
      ]

data RestartPolicy = ExitOnFailure

instance ToJSON RestartPolicy where
  toJSON ExitOnFailure = toJSON ("exit_on_failure" :: Text)

-- | Augmenting sequential-dep edges introduced a cycle. Carries the cycle as
-- the topSort algorithm returned it.
newtype OrderingConflict = OrderingConflict {cycleNodes :: NE.NonEmpty RecipeName}
  deriving stock (Show)

instance Display OrderingConflict where
  displayBuilder (OrderingConflict c) =
    "sequential dependency augmentation introduced a cycle: "
      <> displayBuilder (T.intercalate " -> " (display <$> NE.toList c))

-- | Build the @process-compose@ config for a reachable recipe subset.
--
-- Per recipe @R@ with deps @[d1..dn]@:
--
--   * If @R@ has @[parallel]@ (or has ≤ 1 dep): @R@ depends on every dep
--     directly; siblings are unconstrained, so @process-compose@ runs them
--     concurrently (its default).
--
--   * Otherwise (sequential semantics, which is just's default): @R@ depends
--     only on the last dep; each later dep is augmented to depend on its
--     predecessor, forming a chain @d1 ← d2 ← … ← dn ← R@. Augmenting
--     /shared/ dep nodes is the only way to express caller-side ordering in
--     @process-compose@; the cycle check guards against contradictory chains
--     from different callers.
toProcessCompose ::
  Map.Map RecipeName Recipe ->
  Either OrderingConflict ProcessCompose
toProcessCompose recipes =
  case GA.topSort g of
    Left c -> Left (OrderingConflict c)
    Right _ -> Right $ ProcessCompose $ Map.mapWithKey mkProcess recipes
  where
    g :: G.AdjacencyMap RecipeName
    g = G.overlay (G.vertices (Map.keys recipes)) (G.edges (concatMap callerEdges (Map.toList recipes)))

    mkProcess :: RecipeName -> Recipe -> Process
    mkProcess name _ =
      Process
        { command = "just --no-deps " <> display name,
          depends_on = Map.fromSet (const ProcessCompletedSuccessfully) (G.postSet name g),
          availability = Availability {restart = ExitOnFailure, exit_on_skipped = True}
        }

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
