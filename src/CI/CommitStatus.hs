{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Translate process-compose state events into GitHub commit-status posts.
-- This module owns CI's policy: the @ci/\<recipe\>@ context-name
-- convention, the 'ProcessStatus' → 'CommitStatus' mapping, and the
-- human-readable description per state. The endpoint URL, the wire
-- encoding of each state, and the form-field names are gh-API details
-- owned by "CI.Gh".
module CI.CommitStatus (postStatusFor) where

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

-- | Given a process-compose state event, post the corresponding GitHub
-- commit status under the @ci/\<recipe\>@ context. Non-terminal states
-- ('PsOther') drop on the floor.
--
-- The @gh api@ POST is forked so a slow or hung gh subprocess doesn't
-- back-pressure the subscription loop in
-- 'CI.ProcessCompose.Events.subscribeStates'. Posting failures are logged
-- to stderr with a @gh:@ prefix and swallowed — the recipe's exit code
-- must not depend on whether a status post succeeded.
postStatusFor :: Repo -> Sha -> ProcessState -> IO ()
postStatusFor repo sha ps =
  for_ (psToCommitStatus ps) $ \cs -> void $ forkIO $ do
    let ctx = mkContext ps.name
        post = CommitStatusPost {state = cs, context = ctx, description = describe cs}
    result <- postCommitStatus repo sha post
    let line = "gh: " <> T.unpack (display ctx) <> " " <> T.unpack (display cs)
    case result of
      Right () -> hPutStrLn stderr line
      Left e -> hPutStrLn stderr $ line <> " FAILED: " <> T.unpack (display e)

-- | The single source of truth for status-check context names: @ci/\<recipe\>@.
mkContext :: Display a => a -> Context
mkContext recipe = contextFrom ("ci/" <> display recipe)

-- | CI's human-readable label per state; sent as the @description@ field.
describe :: CommitStatus -> Text
describe Pending = "Running"
describe Success = "Succeeded"
describe Failure = "Failed"
describe Error = "Errored"

psToCommitStatus :: ProcessState -> Maybe CommitStatus
psToCommitStatus ps = case (ps.status, ps.exit_code) of
  (PsRunning, _) -> Just Pending
  (PsCompleted, 0) -> Just Success
  (PsCompleted, _) -> Just Failure
  (PsSkipped, _) -> Just Error
  (PsErrored, _) -> Just Error
  (PsOther _, _) -> Nothing
