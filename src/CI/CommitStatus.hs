{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Translate process-compose state events into GitHub commit-status posts.
-- This module owns CI's policy: the context-name convention (the recipe
-- FQN, used verbatim), the 'ProcessStatus' → 'CommitStatus' mapping, and
-- the human-readable description per state. The endpoint URL, the wire
-- encoding of each state, and the form-field names are gh-API details
-- owned by "CI.Gh". Multi-platform may eventually require a
-- @\<system\>\/\<recipe\>@ shape (see [#14](https://github.com/juspay/ci/issues/14)).
module CI.CommitStatus (postStatusFor, seedPending, logDirFor, logPathFor) where

import CI.Gh (CommitStatus (..), CommitStatusPost (..), Context, Repo, contextFrom, postCommitStatus)
import CI.Git (Sha)
import CI.Justfile (RecipeName)
import CI.ProcessCompose.Events (ProcessState (..), ProcessStatus (..))
import Control.Concurrent.Async (forConcurrently_)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

-- | Given a process-compose state event, post the corresponding GitHub
-- commit status under the @ci/\<recipe\>@ context. Non-terminal states
-- ('PsOther') drop on the floor.
--
-- The status @description@ embeds the path to the recipe's per-run log
-- (@\<logDir\>\/\<recipe\>.log@) so a red check in the GitHub UI carries
-- a navigable pointer to the failing output. The same path is set as
-- the process's @log_location@ in the process-compose YAML, so the
-- file on disk and the path in the status agree by construction.
--
-- Synchronous: each post blocks the subscription loop in
-- 'CI.ProcessCompose.Events.subscribeStates' until @gh api@ returns. This
-- is deliberate — forking the post without tracking the resulting
-- threads loses terminal status posts at process exit (the forked @gh@
-- calls get killed before completing). At our scale (~10 events per run,
-- ~hundreds of ms per post) the brief pause is acceptable; if @gh@
-- hangs ever becomes a real problem, the right fix is a timeout on the
-- post, not a fire-and-forget fork.
--
-- Posting failures are logged to stderr with a @gh:@ prefix and
-- swallowed — the recipe's exit code must not depend on whether a
-- status post succeeded.
postStatusFor :: Repo -> Sha -> FilePath -> ProcessState -> IO ()
postStatusFor repo sha logDir ps =
  for_ (psToCommitStatus ps) $ \cs ->
    postOne repo sha ps.name cs (describe cs (logPathFor logDir ps.name))

-- | Pre-seed every recipe with a 'Pending' commit status at startup —
-- one parallel @gh api@ POST per recipe, all joined before this returns.
-- The PR's checks panel shows the full set of expected checks the moment
-- the pipeline begins, instead of materializing them one at a time as
-- recipes start. Skipped recipes (whose dep failed) get their @pending@
-- overwritten by @error@ when 'postStatusFor' fires; recipes that never
-- run at all stay at @pending@, which surfaces as a visible "why is
-- this still pending?" signal rather than silent absence.
--
-- GitHub has no batch endpoint for commit statuses
-- (see <https://docs.github.com/en/rest/commits/statuses>), so this is
-- N parallel single-status POSTs. 'forConcurrently_' joins all of them
-- before returning, so the caller can rely on every seed being in place
-- before the pipeline kicks off.
seedPending :: Repo -> Sha -> [RecipeName] -> IO ()
seedPending repo sha recipes =
  forConcurrently_ recipes $ \r -> postOne repo sha r Pending "Queued"

-- | Issue one commit-status POST with a caller-supplied description and
-- log the outcome.
postOne :: Display a => Repo -> Sha -> a -> CommitStatus -> Text -> IO ()
postOne repo sha recipe cs desc = do
  let ctx = mkContext recipe
      post = CommitStatusPost {state = cs, context = ctx, description = desc}
  result <- postCommitStatus repo sha post
  let line = "gh: " <> T.unpack (display ctx) <> " " <> T.unpack (display cs)
  case result of
    Right () -> hPutStrLn stderr line
    Left e -> hPutStrLn stderr $ line <> " FAILED: " <> T.unpack (display e)

-- | The single source of truth for status-check context names: the
-- recipe's fully-qualified name, used verbatim.
mkContext :: Display a => a -> Context
mkContext recipe = contextFrom (display recipe)

-- | CI's human-readable label per state, suffixed with the recipe's log
-- path so the GitHub UI's 140-char description carries a one-click
-- pointer to the matching file under @.ci\/\<sha\>\/@. Path stays under
-- ~80 chars at typical recipe-name lengths, leaving room for the state
-- prose without truncation.
describe :: CommitStatus -> FilePath -> Text
describe cs logPath = stateLabel cs <> ": " <> T.pack logPath
  where
    stateLabel Pending = "Running"
    stateLabel Success = "Succeeded"
    stateLabel Failure = "Failed"
    stateLabel Error = "Errored"

-- | Compose @\<logDir\>\/\<recipe\>.log@. The single home for the
-- per-recipe log filename convention; the matching path on the YAML
-- side lives in 'CI.Pipeline.mkLogLocation'.
logPathFor :: Display a => FilePath -> a -> FilePath
logPathFor logDir recipe = logDir </> T.unpack (display recipe) <> ".log"

-- | Compose the per-run log directory: @\<runRoot\>\/\<sha\>\/@. Lives
-- alongside 'logPathFor' so the full @.ci\/\<sha\>\/\<recipe\>.log@
-- convention (directory + filename) is owned by one module — a change
-- to the SHA-keying strategy (short sha, sha+timestamp, content hash)
-- edits one file rather than two.
logDirFor :: FilePath -> Sha -> FilePath
logDirFor runRoot sha = runRoot </> T.unpack (display sha)

psToCommitStatus :: ProcessState -> Maybe CommitStatus
psToCommitStatus ps = case (ps.status, ps.exit_code) of
  (PsRunning, _) -> Just Pending
  (PsCompleted, 0) -> Just Success
  (PsCompleted, _) -> Just Failure
  (PsSkipped, _) -> Just Error
  (PsErrored, _) -> Just Error
  (PsOther _, _) -> Nothing
