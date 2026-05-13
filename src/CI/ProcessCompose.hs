{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The process-compose wire format, in both directions: the YAML config we
-- emit to drive the pipeline, and the JSON events we decode from its
-- @/process/states/ws@ WebSocket stream.
module CI.ProcessCompose
  ( -- * Output schema (YAML config)
    ProcessCompose (..),
    Process (..),
    Dependency (..),
    Condition (..),
    Availability (..),
    RestartPolicy (..),
    toProcessCompose,

    -- * Input schema (WebSocket events)
    ProcessState (..),
    ProcessStatus (..),
    ProcessStateEvent (..),

    -- * Invocation
    processComposeBin,
    UpInvocation (..),
    ServerMode (..),
    toUpArgs,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Justfile (RecipeName)
import Data.Aeson (FromJSON (parseJSON), ToJSON (..), camelTo2, defaultOptions, genericToJSON, withText)
import Data.Aeson.Types (Options (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Which (staticWhich)

-- | Absolute path to the @process-compose@ binary, baked in at compile time
-- via Nix (see @settings.ci.extraBuildTools@ in @flake.nix@).
processComposeBin :: FilePath
processComposeBin = $(staticWhich "process-compose")

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
    mkProcess recipe =
      Process
        { command = mkCommand recipe,
          depends_on = Map.fromSet (const (Dependency ProcessCompletedSuccessfully)) (G.postSet recipe g),
          availability = Availability {restart = ExitOnFailure, exit_on_skipped = True},
          working_dir = T.pack <$> workingDir
        }

-- | The four process-compose states that map onto a GitHub commit status.
-- Everything else (Pending, Launching, Restarting, …) lands in 'PsOther'
-- and is silently dropped by consumers that only care about the four.
-- Typed (rather than left as a raw 'Text') so a typo or an upstream rename
-- becomes a pattern-match exhaustiveness warning instead of a silent miss.
data ProcessStatus
  = PsRunning
  | PsCompleted
  | PsSkipped
  | PsErrored
  | PsOther Text
  deriving stock (Show, Eq)

instance FromJSON ProcessStatus where
  parseJSON = withText "ProcessStatus" $ \t -> pure $ case t of
    "Running" -> PsRunning
    "Completed" -> PsCompleted
    "Skipped" -> PsSkipped
    "Error" -> PsErrored
    other -> PsOther other

-- | Subset of process-compose's @ProcessState@ (per
-- @src\/types\/process.go@) we care about. Aeson ignores extra fields.
data ProcessState = ProcessState
  { name :: Text,
    status :: ProcessStatus,
    exit_code :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | Mirrors process-compose's @ProcessStateEvent@ wire type. We model only
-- the @state@ field; the @snapshot@ flag (true on initial replay, omitted
-- on live transitions) is ignored — aeson drops unknown keys by default,
-- and the observer treats both kinds identically.
newtype ProcessStateEvent = ProcessStateEvent {state :: ProcessState}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | Selects how process-compose's HTTP API surface is exposed (or not).
-- 'NoServer' suppresses the API entirely — used for local-mode dev runs
-- where nothing observes the state. 'UnixSocket' binds the API to a UDS at
-- the given path — used in strict mode where 'CI.Observer' subscribes to
-- the state-event stream over that socket.
data ServerMode
  = NoServer
  | UnixSocket FilePath

-- | All inputs that shape a @process-compose up@ invocation. The YAML
-- config itself goes on stdin separately (it's typically too big for an
-- argv); everything else flag-shaped lives here.
data UpInvocation = UpInvocation
  { server :: ServerMode,
    logFile :: FilePath,
    -- | Caller-supplied extra args appended verbatim after the canned
    -- baseline; used for @--log-level debug@ and similar overrides.
    extraArgs :: [String]
  }

-- | Translate an 'UpInvocation' into the argv vector for @process-compose@.
-- The YAML config is read from stdin (@-f /dev/stdin@) so it's not part of
-- the args. TUI is disabled (@-t=false@) unconditionally — the runner is
-- the only caller and it never wants TUI.
toUpArgs :: UpInvocation -> [String]
toUpArgs up =
  ["up", "-f", "/dev/stdin", "-t=false", "-L", up.logFile] <> serverArgs up.server <> up.extraArgs
  where
    serverArgs NoServer = ["--no-server"]
    serverArgs (UnixSocket path) = ["-U", "-u", path]
