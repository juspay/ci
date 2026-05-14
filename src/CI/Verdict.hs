{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Per-run outcome accumulator and end-of-run verdict. The observer
-- thread folds every terminal 'ProcessState' into an in-memory
-- 'Outcomes' map (keyed by recipe name); after process-compose exits,
-- the orchestrator calls 'runVerdictFrom' on the map to derive the
-- pipeline's overall 'ExitCode' and a printable summary.
--
-- 'RecipeOutcome' is local to this module: it's the verdict's
-- vocabulary, distinct from GitHub's 'CI.Gh.CommitStatus' (which has
-- a @Pending@ "check is open" transition that isn't a terminal
-- outcome). Both vocabularies derive from the same base classifier
-- — 'CI.ProcessCompose.Events.psToTerminalStatus' — so they stay in
-- agreement by construction without this module having to depend on
-- "CI.CommitStatus".
module CI.Verdict
  ( RecipeOutcome (..),
    Outcomes,
    newOutcomes,
    recordOutcome,
    readOutcomes,
    runVerdictFrom,
  )
where

import CI.Justfile (RecipeName)
import CI.ProcessCompose.Events (ProcessState (..), TerminalStatus (..), psToTerminalStatus)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import System.Exit (ExitCode (..))

-- | The terminal outcome of one recipe in a pipeline run. 'Pending'
-- means \"the observer never saw a terminal state event for this
-- recipe\" (the seed value) — distinguishing it from a process that
-- ran and succeeded keeps the verdict honest when pc crashes before
-- scheduling a recipe.
data RecipeOutcome = Pending | Succeeded | Failed | Skipped
  deriving stock (Show, Eq)

instance Display RecipeOutcome where
  displayBuilder Pending = "pending"
  displayBuilder Succeeded = "succeeded"
  displayBuilder Failed = "failed"
  displayBuilder Skipped = "skipped"

-- | The mutable per-run state the observer folds into. Keyed by
-- 'ProcessState.name' (the process-compose-side process name), which
-- for this project's lowering is the recipe's fully-qualified name.
-- Opaque; minted only by 'newOutcomes' and read only by 'readOutcomes'.
newtype Outcomes = Outcomes (IORef (Map Text RecipeOutcome))

-- | Pre-populate the outcome map with 'Pending' for every recipe in
-- the lowered pipeline. Without this, a recipe that pc never emits a
-- state event for (e.g. pc crashed before scheduling it) would be
-- absent from the final map entirely; with it, missing-from-pc
-- surfaces as a lingering 'Pending', which 'runVerdictFrom' treats
-- as a non-success and rolls into a non-zero exit.
newOutcomes :: [RecipeName] -> IO Outcomes
newOutcomes recipes =
  Outcomes <$> newIORef (Map.fromList [(display r, Pending) | r <- recipes])

-- | Fold one 'ProcessState' event into the outcome map. Routes through
-- 'psToTerminalStatus' — the project-wide ground-truth classifier of
-- process-compose's terminal states — and adopts its outcome under
-- the verdict's own vocabulary. Non-terminal events ('PsRunning',
-- 'PsOther') are dropped; the seed 'Pending' stays in place until a
-- real terminal event arrives.
--
-- Safe to call from any thread; the underlying 'atomicModifyIORef''
-- serializes concurrent writes. In practice only the observer thread
-- writes.
recordOutcome :: Outcomes -> ProcessState -> IO ()
recordOutcome (Outcomes ref) ps =
  case terminalToOutcome <$> psToTerminalStatus ps of
    Nothing -> pure ()
    Just o -> atomicModifyIORef' ref (\m -> (Map.insert ps.name o m, ()))

-- | Verdict-side mapping for the three terminal classifications. The
-- isomorphism with 'TerminalStatus' is deliberate (each pc terminal
-- state has a single verdict label) — the rename adopts the
-- verdict's own vocabulary so 'runVerdictFrom''s signature reads in
-- domain terms.
terminalToOutcome :: TerminalStatus -> RecipeOutcome
terminalToOutcome TsSucceeded = Succeeded
terminalToOutcome TsFailed = Failed
terminalToOutcome TsSkipped = Skipped

-- | Snapshot the accumulator. Call once, after the observer subscription
-- has closed (the WebSocket closes when process-compose exits, so by
-- this point every terminal event has been folded in).
readOutcomes :: Outcomes -> IO (Map Text RecipeOutcome)
readOutcomes (Outcomes ref) = readIORef ref

-- | Derive the pipeline's exit code and a printable summary from the
-- accumulated outcomes. The exit code is 0 iff every recipe finished
-- 'Succeeded'; anything else — 'Failed', 'Skipped', or a lingering
-- 'Pending' — flips it to 'ExitFailure' 1.
--
-- Pure: the 'IORef' read happens in the caller; this function takes a
-- snapshot map and is trivial to test against handcrafted inputs.
runVerdictFrom :: Map Text RecipeOutcome -> (ExitCode, [Text])
runVerdictFrom outcomes = (code, summaryLines)
  where
    entries = Map.toAscList outcomes
    failed = filter ((/= Succeeded) . snd) entries
    code
      | null failed = ExitSuccess
      | otherwise = ExitFailure 1
    width = maximum (0 : map (T.length . fst) entries)
    pad n = n <> T.replicate (width - T.length n) " "
    recipeLine (n, o) = "  " <> pad n <> "  " <> display o
    verdictLine
      | null failed = "all " <> tshow (length entries) <> " recipes succeeded"
      | otherwise =
          tshow (length failed)
            <> " of "
            <> tshow (length entries)
            <> " recipes did not succeed"
    summaryLines =
      ["── ci run summary ─────────────────────────────"]
        <> map recipeLine entries
        <> ["───────────────────────────────────────────────", verdictLine]
    tshow = T.pack . show
