{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Translate process-compose state events into GitHub commit-status posts.
-- This module owns CI's policy: the @ci/\<recipe\>@ context-name
-- convention, the 'ProcessStatus' → 'CommitStatus' mapping, and the
-- human-readable description per state. The endpoint URL, the wire
-- encoding of each state, and the form-field names are gh-API details
-- owned by "CI.Gh".
--
-- 'postStatus' issues one POST per transition via 'postCommitStatus' and
-- logs the attempt to stderr with a @gh:@ prefix so it's distinct from
-- per-recipe stdio. Posting failures are logged and swallowed — a flaky
-- API call must not poison the recipe's own exit code.
module CI.CommitStatus (postConsumer) where

import CI.Gh (CommitStatus (..), CommitStatusPost (..), Context, Repo, contextFrom, postCommitStatus)
import CI.Git (Sha)
import CI.ProcessCompose.Events (ProcessState (..), ProcessStatus (..))
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import System.IO (hPutStrLn, stderr)

-- | The single source of truth for status-check context names: @ci/\<recipe\>@.
-- Wraps 'Context' so the prefix lives in exactly one place.
mkContext :: Display a => a -> Context
mkContext recipe = contextFrom ("ci/" <> display recipe)

-- | Issue one commit-status POST and log the attempt to stderr with a
-- @gh:@ prefix. Failures are logged via the 'SubprocessError' 'Display'
-- instance and swallowed — the recipe's exit code must not depend on
-- whether a status post succeeded.
postStatus :: Repo -> Sha -> Context -> CommitStatus -> IO ()
postStatus repo sha ctx cs = do
  result <- postCommitStatus repo sha CommitStatusPost {state = cs, context = ctx, description = describe cs}
  let line = "gh: " <> T.unpack (display ctx) <> " " <> T.unpack (display cs)
  case result of
    Right () -> hPutStrLn stderr line
    Left e -> hPutStrLn stderr $ line <> " FAILED: " <> T.unpack (display e)

-- | CI's human-readable label per state; sent as the @description@ field.
describe :: CommitStatus -> Text
describe Pending = "Running"
describe Success = "Succeeded"
describe Failure = "Failed"
describe Error = "Errored"

-- | Translate each 'ProcessState' into at most one 'postStatus' call
-- under the @ci/\<recipe\>@ context. Non-terminal states ('PsOther') drop
-- on the floor.
--
-- The actual @gh api@ POST is forked so a slow or hung gh subprocess
-- doesn't back-pressure the subscription loop in
-- 'CI.ProcessCompose.Events.subscribeStates' (each post can take hundreds
-- of ms; a burst of state transitions would otherwise serialize behind
-- them).
postConsumer :: Repo -> Sha -> ProcessState -> IO ()
postConsumer repo sha ps =
  for_ (psToCommitStatus ps) $ \cs ->
    void $ forkIO $ postStatus repo sha (mkContext ps.name) cs

psToCommitStatus :: ProcessState -> Maybe CommitStatus
psToCommitStatus ps = case (ps.status, ps.exit_code) of
  (PsRunning, _) -> Just Pending
  (PsCompleted, 0) -> Just Success
  (PsCompleted, _) -> Just Failure
  (PsSkipped, _) -> Just Error
  (PsErrored, _) -> Just Error
  (PsOther _, _) -> Nothing
