{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Per-run outcome accumulator and end-of-run verdict. The observer
thread folds every terminal 'ProcessState' into an in-memory
'Outcomes' map (keyed by 'NodeId' — recipe paired with platform);
after process-compose exits, the orchestrator calls 'verdictCode'
on the map for the pipeline's overall 'ExitCode' and 'verdictSummary'
for the printable summary.

'RecipeOutcome' is local to this module: it's the verdict's
vocabulary, distinct from GitHub's 'CI.Gh.CommitStatus' (which has
a @Pending@ "check is open" transition that isn't a terminal
outcome). Both vocabularies derive from the same base classifier
— 'CI.ProcessCompose.Events.psToTerminalStatus' — so they stay in
agreement by construction without this module having to depend on
"CI.CommitStatus".
-}
module CI.Verdict (
    RecipeOutcome (..),
    Outcomes,
    newOutcomes,
    recordOutcome,
    readOutcomes,
    verdictCode,
    verdictSummary,
    exitWithVerdict,
    terminalToOutcome,
)
where

import CI.Node (NodeId)
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

{- | The terminal outcome of one recipe in a pipeline run. 'Unreported'
means \"the observer never saw a terminal state event for this
recipe\" (the seed value) — distinguishing it from a process that
ran and succeeded keeps the verdict honest when pc crashes before
scheduling a recipe. The name is intentionally distinct from
'CI.Gh.CommitStatus.Pending' (which is the in-flight \"check is
open\" transition GitHub posts during a run) — same word would mean
opposite things in adjacent modules.
-}
data RecipeOutcome = Unreported | Succeeded | Failed | Skipped
    deriving stock (Show, Eq)

instance Display RecipeOutcome where
    displayBuilder Unreported = "unreported"
    displayBuilder Succeeded = "succeeded"
    displayBuilder Failed = "failed"
    displayBuilder Skipped = "skipped"

{- | The mutable per-run state the observer folds into. Keyed by the
typed 'NodeId' so the seed identity and the event-side identity
are the same value (no implicit @Text@ convention to drift). Opaque;
minted only by 'newOutcomes' and read only by 'readOutcomes'.
-}
newtype Outcomes = Outcomes (IORef (Map NodeId RecipeOutcome))

{- | Pre-populate the outcome map with 'Unreported' for every node in
the lowered pipeline. Without this, a node that pc never emits a
state event for (e.g. pc crashed before scheduling it) would be
absent from the final map entirely; with it, missing-from-pc
surfaces as a lingering 'Unreported', which 'verdictCode' treats as
a non-success and rolls into a non-zero exit.
-}
newOutcomes :: [NodeId] -> IO Outcomes
newOutcomes nodes =
    Outcomes <$> newIORef (Map.fromList [(n, Unreported) | n <- nodes])

{- | Fold one 'ProcessState' event for an already-parsed 'NodeId' into
the outcome map. Routes through 'psToTerminalStatus' — the
project-wide ground-truth classifier of process-compose's terminal
states — and adopts its outcome under the verdict's own
vocabulary. Non-terminal events ('PsRunning', 'PsOther') are
dropped; the seed 'Unreported' stays in place until a real
terminal event arrives.

The 'NodeId' is parsed once at the composition site in
'CI.Pipeline'; this module no longer does its own 'parseNodeId'
call. That keeps the two consumers of the state stream ('CI.CommitStatus'
and this one) from independently deciding whether to drop an event
with an unparseable name — there is one parse, one drop decision,
and both downstreams agree by construction.

Safe to call from any thread; the underlying 'atomicModifyIORef''
serializes concurrent writes. In practice only the observer thread
writes.
-}
recordOutcome :: Outcomes -> NodeId -> ProcessState -> IO ()
recordOutcome (Outcomes ref) node ps =
    for_ (terminalToOutcome <$> psToTerminalStatus ps) $ \o ->
        -- 'Map.adjust' silently drops events for nodes the seed doesn't
        -- already know about. That's the right policy: every legitimate
        -- node was seeded by 'newOutcomes', so an unknown key means pc
        -- emitted a state for something we didn't ask it to schedule
        -- (which shouldn't happen, and adding ghost entries to the map
        -- would only confuse the summary).
        atomicModifyIORef' ref (\m -> (Map.adjust (const o) node m, ()))

-- | Verdict-side relabeling of the three terminal classifications.
terminalToOutcome :: TerminalStatus -> RecipeOutcome
terminalToOutcome TsSucceeded = Succeeded
terminalToOutcome TsFailed = Failed
terminalToOutcome TsSkipped = Skipped

{- | End-of-run convenience: snapshot the accumulator, print the
per-recipe summary to stdout, and exit with the derived code.
Glue around 'verdictSummary' + 'verdictCode' for the orchestrator's
one common shape. The pure functions remain the seam for any
caller that wants the summary without exiting (e.g. a future MCP
server or HTTP handler).
-}
exitWithVerdict :: Outcomes -> IO ()
exitWithVerdict outcomes = do
    o <- readOutcomes outcomes
    mapM_ TIO.putStrLn (verdictSummary o)
    exitWith (verdictCode o)

{- | Snapshot the accumulator. Call once, after the observer subscription
has closed (the WebSocket closes when process-compose exits, so by
this point every terminal event has been folded in).
-}
readOutcomes :: Outcomes -> IO (Map NodeId RecipeOutcome)
readOutcomes (Outcomes ref) = readIORef ref

{- | The pipeline's exit code: 'ExitSuccess' iff every node in the
snapshot finished 'Succeeded'; anything else — 'Failed', 'Skipped',
or a lingering 'Unreported' — flips it to 'ExitFailure' 1. Pure;
trivial to test against handcrafted maps.
-}
verdictCode :: Map NodeId RecipeOutcome -> ExitCode
verdictCode outcomes
    | all (== Succeeded) (Map.elems outcomes) = ExitSuccess
    | otherwise = ExitFailure 1

{- | The pipeline's printable per-node summary: a header, one
column-aligned line per node (@\<recipe\>\@\<platform\>@), a
divider, and a one-line verdict count. Pure; companion to
'verdictCode' over the same snapshot.
-}
verdictSummary :: Map NodeId RecipeOutcome -> [Text]
verdictSummary outcomes =
    ["── ci run summary ─────────────────────────────"]
        <> map nodeLine entries
        <> ["───────────────────────────────────────────────", verdictLine]
  where
    entries = [(display n, o) | (n, o) <- Map.toAscList outcomes]
    failedCount = length (filter ((/= Succeeded) . snd) entries)
    width = maximum (0 : map (T.length . fst) entries)
    pad n = n <> T.replicate (width - T.length n) " "
    nodeLine (n, o) = "  " <> pad n <> "  " <> display o
    verdictLine
        | failedCount == 0 = "all " <> tshow (length entries) <> " nodes succeeded"
        | otherwise =
            tshow failedCount
                <> " of "
                <> tshow (length entries)
                <> " nodes did not succeed"
    tshow = T.pack . show
