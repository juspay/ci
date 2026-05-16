{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Per-run outcome accumulator and end-of-run verdict. The observer
-- thread folds every terminal 'ProcessState' into an in-memory
-- 'Outcomes' map (keyed by 'NodeId' — recipe paired with platform);
-- after process-compose exits, the orchestrator calls 'verdictCode'
-- on the map for the pipeline's overall 'ExitCode' and 'verdictSummary'
-- for the printable summary.
--
-- 'RecipeOutcome' is the verdict's vocabulary: two terminal cases.
-- Absence of a node from the event map (encoded as 'Nothing' in the
-- pre-seeded map below) means "the observer never saw a terminal
-- state for this node" — a non-success that flows into 'verdictCode'
-- without needing its own 'RecipeOutcome' constructor.
module CI.Verdict
  ( -- * Outcome values
    RecipeOutcome (..),
    Outcomes,

    -- * Per-event accumulator
    newOutcomes,
    recordOutcome,
    readOutcomes,

    -- * End-of-run summary
    verdictCode,
    verdictSummary,
    exitWithVerdict,

    -- * === Internal (test surface) ===
    terminalToOutcome,
    -- ^ Exposed only for "test.CI.VerdictSpec"'s cross-module
    -- agreement check against 'CI.CommitStatus.terminalToCommitStatus'
    -- — production code reaches the mapping through 'recordOutcome'.
  )
where

import CI.Node (NodeId (..))
import CI.ProcessCompose.Events (ProcessState (..), TerminalStatus (..), psToTerminalStatus)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (..), exitWith)

-- | The terminal outcome of one node: ran-and-succeeded, or didn't.
-- "Didn't reach a terminal event at all" (pc crashed before
-- scheduling, network drop on the observer) is *not* a 'RecipeOutcome'
-- constructor — it's the absence of a fold value in the per-node
-- 'Maybe' slot of the 'Outcomes' map. "Skipped because the upstream
-- failed" isn't a constructor either — that's a graph property of
-- the dep map combined with the outcomes, not a per-node primitive.
data RecipeOutcome = Succeeded | Failed
  deriving stock (Show, Eq)

instance Display RecipeOutcome where
  displayBuilder Succeeded = "succeeded"
  displayBuilder Failed = "failed"

-- | The mutable per-run state the observer folds into. Each scheduled
-- 'NodeId' starts with 'Nothing' (no terminal event yet) and flips to
-- @Just outcome@ when 'recordOutcome' fires. Keyed by the typed 'NodeId'
-- so the seed identity and the event-side identity are the same value
-- (no implicit @Text@ convention to drift). Opaque; minted only by
-- 'newOutcomes' and read only by 'readOutcomes'.
newtype Outcomes = Outcomes (IORef (Map NodeId (Maybe RecipeOutcome)))

-- | Pre-populate the outcome map with 'Nothing' for every node in
-- the lowered pipeline. Without this, a node that pc never emits a
-- state event for (e.g. pc crashed before scheduling it) would be
-- absent from the final map entirely; with it, missing-from-pc
-- surfaces as a 'Nothing', which 'verdictCode' treats as a
-- non-success and rolls into a non-zero exit.
newOutcomes :: [NodeId] -> IO Outcomes
newOutcomes nodes =
  Outcomes <$> newIORef (Map.fromList [(n, Nothing) | n <- nodes])

-- | Fold one 'ProcessState' event for an already-parsed 'NodeId' into
-- the outcome map. Routes through 'psToTerminalStatus' — the
-- project-wide ground-truth classifier of process-compose's terminal
-- states — and adopts its outcome under the verdict's own
-- vocabulary. Non-terminal events ('PsRunning', 'PsOther') are
-- dropped; the seed 'Nothing' stays in place until a real terminal
-- event arrives.
--
-- The 'NodeId' is parsed once at the composition site in
-- 'CI.Pipeline'; this module no longer does its own 'parseNodeId'
-- call. That keeps the two consumers of the state stream ('CI.CommitStatus'
-- and this one) from independently deciding whether to drop an event
-- with an unparseable name — there is one parse, one drop decision,
-- and both downstreams agree by construction.
--
-- Safe to call from any thread; the underlying 'atomicModifyIORef''
-- serializes concurrent writes. In practice only the observer thread
-- writes.
recordOutcome :: Outcomes -> NodeId -> ProcessState -> IO ()
recordOutcome (Outcomes ref) node ps =
  for_ (terminalToOutcome <$> psToTerminalStatus ps) $ \o ->
    -- 'Map.adjust' silently drops events for nodes the seed doesn't
    -- already know about. That's the right policy: every legitimate
    -- node was seeded by 'newOutcomes', so an unknown key means pc
    -- emitted a state for something we didn't ask it to schedule
    -- (which shouldn't happen, and adding ghost entries to the map
    -- would only confuse the summary).
    atomicModifyIORef' ref (\m -> (Map.adjust (const (Just o)) node m, ()))

-- | Verdict-side relabeling of the two terminal classifications.
terminalToOutcome :: TerminalStatus -> RecipeOutcome
terminalToOutcome TsSucceeded = Succeeded
terminalToOutcome TsFailed = Failed

-- | End-of-run convenience: snapshot the accumulator, print the
-- per-recipe summary to stdout, and exit with the derived code.
-- Glue around 'verdictSummary' + 'verdictCode' for the orchestrator's
-- one common shape. The pure functions remain the seam for any
-- caller that wants the summary without exiting (e.g. a future MCP
-- server or HTTP handler).
--
-- @mkHost@ resolves the host each node ran on — the orchestrator
-- threads this in from the loaded 'CI.Hosts.Hosts' so the verdict
-- module doesn't take a direct dependency on the hosts vocabulary.
exitWithVerdict :: (NodeId -> Text) -> Outcomes -> IO ()
exitWithVerdict mkHost outcomes = do
  o <- readOutcomes outcomes
  mapM_ TIO.putStrLn (verdictSummary mkHost o)
  exitWith (verdictCode o)

-- | Snapshot the accumulator. Call once, after the observer subscription
-- has closed (the WebSocket closes when process-compose exits, so by
-- this point every terminal event has been folded in).
readOutcomes :: Outcomes -> IO (Map NodeId (Maybe RecipeOutcome))
readOutcomes (Outcomes ref) = readIORef ref

-- | The pipeline's exit code: 'ExitSuccess' iff every node in the
-- snapshot finished @'Just' 'Succeeded'@; anything else — @Just Failed@
-- or 'Nothing' (no terminal event) — flips it to 'ExitFailure' 1.
-- Pure; trivial to test against handcrafted maps.
verdictCode :: Map NodeId (Maybe RecipeOutcome) -> ExitCode
verdictCode outcomes
  | all (== Just Succeeded) (Map.elems outcomes) = ExitSuccess
  | otherwise = ExitFailure 1

-- | The pipeline's printable per-node summary: a header, one
-- column-aligned line per node (@\<recipe\>\@\<platform\>  \<host\>  \<outcome\>@), a
-- divider, and a one-line verdict count. Pure; companion to
-- 'verdictCode' over the same snapshot.
--
-- The host column shows where each node ran ("local" for the
-- orchestrator-local lane, the SSH host name otherwise). Caller
-- supplies the resolver so this module doesn't depend on the
-- "CI.Hosts" vocabulary directly.
--
-- A 'Nothing' outcome — node was scheduled but never reached a
-- terminal event — renders as @"did not run"@. This covers both
-- "pc crashed before scheduling" and the upstream-cascade case
-- (when pc emits @Skipped@ events for dep-failed nodes,
-- 'psToTerminalStatus' folds them to 'TsFailed' if pc still routed
-- them; nodes pc never scheduled at all stay 'Nothing').
--
-- Synthetic setup nodes ('SetupNode') are filtered out of the
-- per-node lines and the @n of m@ count: they're internal plumbing
-- (per-platform bundle ship, drv copy), not user recipes. This
-- matches 'CI.CommitStatus.seedPending' / 'CI.CommitStatus.postStatusFor',
-- which skip setup nodes from GitHub commit-status posts — so the PR
-- reviewer on GitHub and the local CLI user see the same set of
-- user-facing nodes.
verdictSummary :: (NodeId -> Text) -> Map NodeId (Maybe RecipeOutcome) -> [Text]
verdictSummary mkHost outcomes =
  ["── ci run summary ─────────────────────────────"]
    <> map nodeLine entries
    <> ["───────────────────────────────────────────────", verdictLine]
  where
    userNodes = Map.filterWithKey (\n _ -> isRecipe n) outcomes
    isRecipe (RecipeNode _ _) = True
    isRecipe (SetupNode _) = False
    entries = [(display n, mkHost n, o) | (n, o) <- Map.toAscList userNodes]
    failedCount = length (filter (\(_, _, o) -> o /= Just Succeeded) entries)
    nodeWidth = maximum (0 : [T.length n | (n, _, _) <- entries])
    hostWidth = maximum (0 : [T.length h | (_, h, _) <- entries])
    pad w t = t <> T.replicate (w - T.length t) " "
    renderOutcome (Just o) = display o
    renderOutcome Nothing = "did not run"
    nodeLine (n, h, o) = "  " <> pad nodeWidth n <> "  " <> pad hostWidth h <> "  " <> renderOutcome o
    verdictLine
      | failedCount == 0 = "all " <> tshow (length entries) <> " nodes succeeded"
      | otherwise =
          tshow failedCount
            <> " of "
            <> tshow (length entries)
            <> " nodes did not succeed"
    tshow = T.pack . show
