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
    Dependency (..),
    Condition (..),
    Availability (..),
    RestartPolicy (..),

    -- * Assembly
    toProcessCompose,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Justfile (RecipeName)
import Data.Aeson (ToJSON (..), camelTo2, defaultOptions, genericToJSON)
import Data.Aeson.Types (Options (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Aeson 'Options' that translate @CamelCase@ constructor tags into
-- @snake_case@, matching process-compose's wire conventions
-- (@ExitOnFailure@ → @"exit_on_failure"@). 'tagSingleConstructors' is on
-- so single-constructor nullary sums still go through the sum encoding
-- and emit their tag as a string rather than aeson's default empty array.
snakeCaseTag :: Options
snakeCaseTag =
  defaultOptions
    { constructorTagModifier = camelTo2 '_',
      tagSingleConstructors = True
    }

-- | Top-level @process-compose.yaml@: a map from process name to spec.
newtype ProcessCompose = ProcessCompose {processes :: Map.Map RecipeName Process}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | One @processes.<name>@ entry. Field names match @process-compose@'s YAML keys.
data Process = Process
  { command :: Text,
    depends_on :: Map.Map RecipeName Dependency,
    availability :: Availability,
    -- | When set, process-compose @chdir@s the spawned recipe into this
    -- directory before executing 'command'. Used in strict mode to pin
    -- every recipe to an immutable @git worktree@ snapshot of HEAD.
    -- 'Nothing' omits the field from the YAML so dev runs are unchanged.
    working_dir :: Maybe Text
  }
  deriving stock (Generic)

-- Custom instance so 'Nothing' in 'working_dir' drops the field entirely
-- rather than emitting @working_dir: null@.
instance ToJSON Process where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

-- | One entry inside @depends_on@: the condition under which the named
-- dependency is considered satisfied.
data Dependency = Dependency
  { condition :: Condition
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | Process-compose's set of dep-edge conditions. We only emit
-- 'ProcessCompletedSuccessfully' today; the closed sum names every value the
-- wire format admits so the choice stays type-safe at every emission site.
data Condition
  = ProcessCompletedSuccessfully
  deriving stock (Generic)

instance ToJSON Condition where
  toJSON = genericToJSON snakeCaseTag

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

-- | Restart strategy for a process. We only emit one variant today; the
-- closed sum keeps the choice explicit at the call site.
data RestartPolicy = ExitOnFailure
  deriving stock (Generic)

instance ToJSON RestartPolicy where
  toJSON = genericToJSON snakeCaseTag

-- | Assemble a @process-compose@ config from a pre-validated execution graph.
-- The caller supplies @mkCommand@, the shell command emitted for each
-- vertex, and @workingDir@, the directory every recipe is @chdir@'d into
-- (or 'Nothing' to leave the field off and let process-compose use its
-- caller's cwd). Each outgoing edge becomes a @depends_on@ entry. Keeping
-- both decisions out of this module lets callers vary how vertices are
-- invoked and where they execute without the YAML emitter knowing about
-- any of those policies.
toProcessCompose :: Maybe FilePath -> (RecipeName -> Text) -> G.AdjacencyMap RecipeName -> ProcessCompose
toProcessCompose workingDir mkCommand g =
  ProcessCompose $ Map.fromSet mkProcess (G.vertexSet g)
  where
    mkProcess name =
      Process
        { command = mkCommand name,
          depends_on = Map.fromSet (const (Dependency ProcessCompletedSuccessfully)) (G.postSet name g),
          availability = Availability {restart = ExitOnFailure, exit_on_skipped = True},
          working_dir = T.pack <$> workingDir
        }
