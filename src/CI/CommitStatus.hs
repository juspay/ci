{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | GitHub commit-status wire format and the @gh@ CLI adapter. A migration
-- to @check-runs@, or to a different backend entirely (Bitbucket, Slack),
-- swaps this module — but the resolvers in "CI.Resolve" and the observer in
-- "CI.Observer" don't move.
--
-- 'postStatus' issues a single REST call per transition and logs the
-- attempt to stderr with a @gh:@ prefix so the output is visually distinct
-- from per-recipe stdio. Posting failures are logged and swallowed — a
-- flaky API call must not poison the recipe's own exit code.
module CI.CommitStatus
  ( -- * Wire vocabulary
    CommitStatus (..),
    Context,
    mkContext,

    -- * Posting
    postStatus,
    postConsumer,
  )
where

import CI.Observer (ProcessState (..), ProcessStatus (..))
import CI.Resolve (RepoCoords (..), Sha (..))
import Data.Foldable (for_)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import System.Which (staticWhich)

ghBin :: FilePath
ghBin = $(staticWhich "gh")

-- | A GitHub status-check context (the unique label that groups posts of the
-- same check, shown on the PR's checks panel). Construct via 'mkContext';
-- the constructor is intentionally hidden so the @ci/\<recipe\>@ naming
-- convention lives in one place.
newtype Context = Context Text
  deriving newtype (Show, Eq, IsString)

-- | The single source of truth for status-check context names: @ci/\<recipe\>@.
mkContext :: Display a => a -> Context
mkContext name = Context ("ci/" <> display name)

-- | The four GitHub commit-status states. The observer maps process-compose
-- events to these: @Running@→'Pending', @Completed@+exit0→'Success',
-- @Completed@+exit≠0→'Failure', @Skipped@/@Error@→'Error'.
data CommitStatus = Pending | Success | Failure | Error
  deriving stock (Show, Eq)

postStatus :: RepoCoords -> Sha -> Context -> CommitStatus -> IO ()
postStatus coords (Sha sha) (Context ctx) cs = do
  let endpoint =
        "/repos/"
          <> T.unpack coords.owner
          <> "/"
          <> T.unpack coords.repo
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
  (ec, _, ghStderr) <- readProcessWithExitCode ghBin args ""
  let line = "gh: " <> T.unpack ctx <> " " <> T.unpack (wireState cs)
  case ec of
    ExitSuccess -> hPutStrLn stderr line
    ExitFailure n ->
      hPutStrLn stderr $ line <> " FAILED (" <> show n <> "): " <> ghStderr

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
postConsumer :: RepoCoords -> Sha -> ProcessState -> IO ()
postConsumer coords sha ps =
  for_ (psToCommitStatus ps) (postStatus coords sha (mkContext (name ps)))

psToCommitStatus :: ProcessState -> Maybe CommitStatus
psToCommitStatus ps = case (status ps, exit_code ps) of
  (PsRunning, _) -> Just Pending
  (PsCompleted, 0) -> Just Success
  (PsCompleted, _) -> Just Failure
  (PsSkipped, _) -> Just Error
  (PsErrored, _) -> Just Error
  (PsOther _, _) -> Nothing
