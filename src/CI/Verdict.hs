{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Per-run outcome accumulator and end-of-run verdict. The observer
-- thread folds every terminal 'ProcessState' into an in-memory
-- 'Outcomes' map (keyed by recipe name); after process-compose exits,
-- the orchestrator calls 'runVerdictFrom' on the map to derive the
-- pipeline's overall 'ExitCode' and a printable summary.
--
-- Why this lives outside "CI.Pipeline": the orchestrator owns the
-- run-sequence axis (clean-tree → worktree → spawn → wait → exit) and
-- the verdict owns a different axis — what counts as failure, how the
-- per-recipe summary is rendered. Splitting the two keeps either from
-- growing the other's complexity.
--
-- Why this lives outside "CI.CommitStatus": that module owns the GitHub
-- wire mapping ('ProcessState' → 'CommitStatus' POST). The accumulator
-- consumes 'CommitStatus' values (reusing 'psToCommitStatus' so both
-- the GH posts and the local verdict agree on the same predicate)
-- but adds nothing to the GH-posting story.
module CI.Verdict
  ( Outcomes,
    newOutcomes,
    recordOutcome,
    readOutcomes,
    runVerdictFrom,
  )
where

import CI.CommitStatus (psToCommitStatus)
import CI.Gh (CommitStatus (..))
import CI.Justfile (RecipeName)
import CI.ProcessCompose.Events (ProcessState (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)
import System.Exit (ExitCode (..))

-- | The mutable per-run state the observer folds into. Keyed by
-- 'ProcessState.name' (the process-compose-side process name), which
-- for this project's lowering is the recipe's fully-qualified name.
-- Opaque; minted only by 'newOutcomes' and read only by 'readOutcomes'.
newtype Outcomes = Outcomes (IORef (Map Text CommitStatus))

-- | Pre-populate the outcome map with @Pending@ for every recipe in
-- the lowered pipeline. Without this, a recipe that pc never emits a
-- state event for (e.g. pc crashed before scheduling it) would be
-- absent from the final map entirely; with it, missing-from-pc
-- surfaces as a lingering 'Pending', which 'runVerdictFrom' treats
-- as a non-success and rolls into a non-zero exit.
newOutcomes :: [RecipeName] -> IO Outcomes
newOutcomes recipes =
  Outcomes <$> newIORef (Map.fromList [(display r, Pending) | r <- recipes])

-- | Fold one 'ProcessState' event into the outcome map. Non-terminal
-- events (those for which 'psToCommitStatus' returns 'Nothing') are
-- dropped; terminal events overwrite the recipe's slot. This is the
-- same derivation 'CI.CommitStatus.postStatusFor' uses for its GH
-- post, so the local verdict and the GH check page agree by
-- construction.
--
-- Safe to call from any thread; the underlying 'atomicModifyIORef''
-- serializes concurrent writes. In practice only the observer thread
-- writes.
recordOutcome :: Outcomes -> ProcessState -> IO ()
recordOutcome (Outcomes ref) ps =
  case psToCommitStatus ps of
    Nothing -> pure ()
    Just cs -> atomicModifyIORef' ref (\m -> (Map.insert ps.name cs m, ()))

-- | Snapshot the accumulator. Call once, after the observer subscription
-- has closed (the WebSocket closes when process-compose exits, so by
-- this point every terminal event has been folded in).
readOutcomes :: Outcomes -> IO (Map Text CommitStatus)
readOutcomes (Outcomes ref) = readIORef ref

-- | Derive the pipeline's exit code and a printable summary from the
-- accumulated outcomes. The exit code is 0 iff every recipe finished
-- 'Success'; anything else — 'Failure', 'Error' (skipped), or a
-- lingering 'Pending' — flips it to 'ExitFailure' 1.
--
-- Pure: the 'IORef' read happens in the caller; this function takes a
-- snapshot map and is trivial to test against handcrafted inputs.
runVerdictFrom :: Map Text CommitStatus -> (ExitCode, [Text])
runVerdictFrom outcomes = (code, summaryLines)
  where
    entries = Map.toAscList outcomes
    failed = filter ((/= Success) . snd) entries
    code
      | null failed = ExitSuccess
      | otherwise = ExitFailure 1
    width = maximum (0 : map (T.length . fst) entries)
    pad n = n <> T.replicate (width - T.length n) " "
    recipeLine (n, cs) = "  " <> pad n <> "  " <> display cs
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
