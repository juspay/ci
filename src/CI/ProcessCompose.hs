{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Assemble a @process-compose@ YAML config from a pre-validated execution graph.
module CI.ProcessCompose
  ( -- * Schema
    ProcessCompose (..),
    Process (..),
    Availability (..),
    RestartPolicy (..),

    -- * Assembly
    toProcessCompose,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Justfile (RecipeName)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Display (display)
import GHC.Generics (Generic)

-- | Top-level @process-compose.yaml@: a map from process name to spec.
newtype ProcessCompose = ProcessCompose {processes :: Map.Map RecipeName Process}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | One @processes.<name>@ entry. Field names match @process-compose@'s YAML keys.
data Process = Process
  { command :: Text,
    depends_on :: Set RecipeName,
    availability :: Availability
  }

-- | Every dep edge we emit carries the same condition.
processCompletedSuccessfully :: Text
processCompletedSuccessfully = "process_completed_successfully"

instance ToJSON Process where
  toJSON p =
    object
      [ "command" .= p.command,
        "depends_on" .= Map.fromSet (const (object ["condition" .= processCompletedSuccessfully])) p.depends_on,
        "availability" .= p.availability
      ]

-- | Per-process failure policy. Both knobs are set explicitly: 'ExitOnFailure'
-- propagates a non-zero exit upward; 'exit_on_skipped' also tears down when a
-- dep is skipped (which would otherwise leave the pipeline hanging with no
-- failure signal at the parent).
data Availability = Availability
  { restart :: RestartPolicy,
    exit_on_skipped :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data RestartPolicy = ExitOnFailure

instance ToJSON RestartPolicy where
  toJSON ExitOnFailure = toJSON ("exit_on_failure" :: Text)

-- | Assemble a @process-compose@ config from a pre-validated execution graph.
-- Each vertex becomes a process invoking @just --no-deps <name>@; each
-- outgoing edge becomes a @depends_on@ entry.
toProcessCompose :: G.AdjacencyMap RecipeName -> ProcessCompose
toProcessCompose g =
  ProcessCompose $ Map.fromSet mkProcess (G.vertexSet g)
  where
    mkProcess name =
      Process
        { command = "just --no-deps " <> display name,
          depends_on = G.postSet name g,
          availability = Availability {restart = ExitOnFailure, exit_on_skipped = True}
        }
