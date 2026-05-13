{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import CI.Gh (CommitStatus (..), CommitStatusPost (..), Repo, postCommitStatus)
import CI.Git (Sha)
import CI.ProcessCompose (ProcessState (..), ProcessStatus (..))
import qualified CI.Subprocess as Sub
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import System.IO (hPutStrLn, stderr)

-- | A GitHub status-check context (the unique label that groups posts of the
-- same check, shown on the PR's checks panel). Construct via 'mkContext';
-- the constructor is intentionally hidden so the @ci/\<recipe\>@ naming
-- convention lives in one place.
newtype Context = Context Text
  deriving newtype (Show)

-- | The single source of truth for status-check context names: @ci/\<recipe\>@.
mkContext :: Display a => a -> Context
mkContext recipe = Context ("ci/" <> display recipe)

-- | Issue one commit-status POST and log the attempt to stderr with a
-- @gh:@ prefix. Failures are logged with @FAILED@ and the exit code,
-- never propagated — the recipe's exit code must not depend on whether a
-- status post succeeded.
postStatus :: Repo -> Sha -> Context -> CommitStatus -> IO ()
postStatus repo sha (Context ctx) cs = do
  result <- postCommitStatus repo sha CommitStatusPost {state = cs, context = ctx, description = describe cs}
  let line = "gh: " <> T.unpack ctx <> " " <> T.unpack (display cs)
  case result of
    Right () -> hPutStrLn stderr line
    Left e ->
      hPutStrLn stderr $ line <> " FAILED (" <> show e.code <> "): " <> Sub.stderr e

-- | CI's human-readable label per state; sent as the @description@ field.
describe :: CommitStatus -> Text
describe Pending = "Running"
describe Success = "Succeeded"
describe Failure = "Failed"
describe Error = "Errored"

-- | An observer consumer (see "CI.Observer") that translates each
-- 'ProcessState' into at most one 'postStatus' call under the
-- @ci/\<recipe\>@ context. Non-terminal states ('PsOther') drop on the
-- floor.
postConsumer :: Repo -> Sha -> ProcessState -> IO ()
postConsumer repo sha ps =
  for_ (psToCommitStatus ps) (postStatus repo sha (mkContext ps.name))

psToCommitStatus :: ProcessState -> Maybe CommitStatus
psToCommitStatus ps = case (ps.status, ps.exit_code) of
  (PsRunning, _) -> Just Pending
  (PsCompleted, 0) -> Just Success
  (PsCompleted, _) -> Just Failure
  (PsSkipped, _) -> Just Error
  (PsErrored, _) -> Just Error
  (PsOther _, _) -> Nothing
