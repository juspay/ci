{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The config-and-invocation half of the process-compose interface: the
-- YAML schema we emit, the @up@-invocation argv, and the spawn-and-wait
-- operation. The event-stream half (WS-over-UDS subscription, typed
-- @ProcessState@/@ProcessStatus@) lives in "CI.ProcessCompose.Events" —
-- those wire vocabularies change on a different axis from the YAML
-- config.
module CI.ProcessCompose
  ( -- * Output schema (YAML config)
    ProcessCompose,
    toProcessCompose,
    processNames,

    -- * Invocation
    UpInvocation (..),
    runProcessCompose,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Justfile (RecipeName)
import Data.Aeson (ToJSON (..), camelTo2, defaultOptions, genericToJSON)
import Data.Aeson.Types (Options (..))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Yaml as Y
import GHC.Generics (Generic)
import System.Exit (ExitCode, die)
import System.IO (hClose)
import System.Process (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)
import System.Which (staticWhich)

-- | Absolute path to the @process-compose@ binary, baked in at compile time
-- via Nix (see @settings.ci.extraBuildTools@ in @flake.nix@). Not exported:
-- the only spawn site is 'runProcessCompose' below.
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
    working_dir :: Maybe FilePath
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

-- | Per-process failure policy. Both knobs are set explicitly so the
-- value at every emission site shows the policy in full: 'RestartNo'
-- lets a failed recipe stay failed without tearing the project down,
-- and @exit_on_skipped = False@ keeps the same composure when a
-- downstream recipe is skipped because its dep failed. The
-- combination is what gives sibling lanes the freedom to keep running
-- after one fails — verifying the cross-lane outcome is then the job
-- of "CI.Verdict", not of process-compose's own exit code.
data Availability = Availability
  { restart :: RestartPolicy,
    exit_on_skipped :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | Restart strategy for a process. We only emit 'No' today — "let the
-- failure stick, surface it in the verdict" — but the closed sum
-- names every value the wire format admits ('ExitOnFailure' would
-- shut the whole project down on the first failure) so the choice
-- stays type-safe at the emission site.
data RestartPolicy = No | ExitOnFailure
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
          availability = Availability {restart = No, exit_on_skipped = False},
          working_dir = workingDir
        }

-- | The set of recipe names in a 'ProcessCompose'. Returned in 'Map' key
-- order so iteration is stable. Useful for pre-seeding per-recipe state
-- at startup (e.g. posting @pending@ commit statuses for every recipe
-- before process-compose has begun scheduling them).
processNames :: ProcessCompose -> [RecipeName]
processNames (ProcessCompose ps) = Map.keys ps

-- | All inputs that shape a @process-compose up@ invocation. The YAML
-- config itself goes on stdin separately (it's typically too big for an
-- argv); everything else flag-shaped lives here. Process-compose always
-- binds its API to a UDS at @sockPath@ — that's both the
-- 'CI.ProcessCompose.Events.subscribeStates' attachment point and the
-- de-facto mutex for "is a ci run in progress in this checkout."
data UpInvocation = UpInvocation
  { sockPath :: FilePath,
    logFile :: FilePath,
    -- | Caller-supplied args appended verbatim after the canned
    -- baseline; the @ci run -- ...@ passthrough lands here.
    passthroughArgs :: [String]
  }

-- | Translate an 'UpInvocation' into the argv vector for @process-compose@.
-- The YAML config is read from stdin (@-f /dev/stdin@) so it's not part of
-- the args. TUI is disabled (@-t=false@) unconditionally — there is only
-- one caller ('runProcessCompose') and it never wants TUI.
toUpArgs :: UpInvocation -> [String]
toUpArgs up =
  ["up", "-f", "/dev/stdin", "-t=false", "-L", up.logFile, "-U", "-u", up.sockPath] <> up.passthroughArgs

-- | Spawn @process-compose up@ from the 'UpInvocation', encode the
-- 'ProcessCompose' as YAML on stdin, and forward the subprocess's exit
-- code. 'withCreateProcess' brackets the spawn so stdin is closed and the
-- child reaped even if 'BS.hPut' throws (e.g. broken pipe).
runProcessCompose :: UpInvocation -> ProcessCompose -> IO ExitCode
runProcessCompose up pc =
  withCreateProcess cp $ \mhin _ _ ph -> case mhin of
    Nothing -> die "process-compose: stdin pipe was not created"
    Just hin -> do
      BS.hPut hin (Y.encode pc)
      hClose hin
      waitForProcess ph
  where
    cp = (proc processComposeBin (toUpArgs up)) {std_in = CreatePipe}

