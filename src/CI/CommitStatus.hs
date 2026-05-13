{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | GitHub commit-status wire format and the @gh@ CLI adapter. 'postStatus'
-- issues a single REST call per transition and logs the attempt to stderr
-- with a @gh:@ prefix so the output is visually distinct from per-recipe
-- stdio. Posting failures are logged and swallowed — a flaky API call
-- must not poison the recipe's own exit code.
module CI.CommitStatus (postConsumer) where

import CI.Gh (Repo (..), ghBin)
import CI.Git (Sha (..))
import CI.ProcessCompose (ProcessState (..), ProcessStatus (..))
import CI.Subprocess (runSubprocess)
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

-- | The four GitHub commit-status states. The observer maps process-compose
-- events to these: @Running@→'Pending', @Completed@+exit0→'Success',
-- @Completed@+exit≠0→'Failure', @Skipped@/@Error@→'Error'.
data CommitStatus = Pending | Success | Failure | Error
  deriving stock (Show, Eq)

-- | Issue one @gh api POST /repos/{owner}/{repo}/statuses/{sha}@ call and
-- log the attempt to stderr with a @gh:@ prefix. Failures are logged with
-- @FAILED@ and the exit code, never propagated — the recipe's exit code
-- must not depend on whether a status post succeeded.
postStatus :: Repo -> Sha -> Context -> CommitStatus -> IO ()
postStatus repo (Sha sha) (Context ctx) cs = do
  let endpoint =
        "/repos/"
          <> T.unpack repo.owner
          <> "/"
          <> T.unpack repo.name
          <> "/statuses/"
          <> T.unpack sha
      args =
        [ "api",
          "-X",
          "POST",
          endpoint,
          "-f",
          "state=" <> T.unpack (wireState cs),
          "-f",
          "context=" <> T.unpack ctx,
          "-f",
          "description=" <> T.unpack (wireDescription cs)
        ]
  result <- runSubprocess ("gh api statuses " <> wireState cs) ghBin args ""
  let line = "gh: " <> T.unpack ctx <> " " <> T.unpack (wireState cs)
  case result of
    Right _ -> hPutStrLn stderr line
    Left e ->
      hPutStrLn stderr $ line <> " FAILED (" <> show e.code <> "): " <> Sub.stderr e

wireState :: CommitStatus -> Text
wireState Pending = "pending"
wireState Success = "success"
wireState Failure = "failure"
wireState Error = "error"

wireDescription :: CommitStatus -> Text
wireDescription Pending = "Running"
wireDescription Success = "Succeeded"
wireDescription Failure = "Failed"
wireDescription Error = "Errored"

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
