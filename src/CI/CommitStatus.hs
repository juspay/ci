{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | GitHub commit-status wire format and the @gh@ CLI adapter. Repo
-- coordinates and HEAD SHA are resolved once at the top of @main@'s @Run@
-- branch and kept in scope for the duration of the run; the central
-- observer ('CI.Observer') translates process-compose state events into
-- 'postStatus' calls.
--
-- 'postStatus' issues a single REST call per transition and logs the
-- attempt to stderr with a @gh:@ prefix so the output is visually distinct
-- from per-recipe stdio. Posting failures are logged and swallowed — a
-- flaky API call must not poison the recipe's own exit code.
module CI.CommitStatus
  ( -- * Errors
    ResolveError (..),
    ensureCleanTree,

    -- * Resolved coordinates
    RepoCoords (..),
    Sha (..),
    Context,
    mkContext,
    resolveRepoCoords,
    resolveSha,

    -- * Wire vocabulary + posting
    CommitStatus (..),
    postStatus,
  )
where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import System.Which (staticWhich)

ghBin :: FilePath
ghBin = $(staticWhich "gh")

gitBin :: FilePath
gitBin = $(staticWhich "git")

data RepoCoords = RepoCoords {owner :: Text, repo :: Text}
  deriving stock (Show, Eq)

newtype Sha = Sha Text
  deriving newtype (Show, Eq, IsString)

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

-- | Failures from the resolve / clean-tree checks performed at the top of
-- strict mode. Kept as a single sum so 'main' has one type to display + die
-- on, and so a future caller that wants to fall back instead of dying can
-- pattern-match without parsing strings.
data ResolveError
  = ResolveShaFailed Int String
  | ResolveRepoFailed Int String
  | UnexpectedNameWithOwner String
  | GitStatusFailed Int String
  | -- | Working tree has uncommitted changes; carries the @git status
    -- --porcelain@ lines so the user can see what's dirty.
    DirtyTree [Text]
  deriving stock (Show)

instance Display ResolveError where
  displayBuilder (ResolveShaFailed n err) =
    "git rev-parse HEAD failed (" <> displayBuilder n <> "): " <> displayBuilder (T.pack err)
  displayBuilder (ResolveRepoFailed n err) =
    "gh repo view failed (" <> displayBuilder n <> "): " <> displayBuilder (T.pack err)
  displayBuilder (UnexpectedNameWithOwner out) =
    "unexpected nameWithOwner from gh: " <> displayBuilder (T.pack out)
  displayBuilder (GitStatusFailed n err) =
    "git status --porcelain failed (" <> displayBuilder n <> "): " <> displayBuilder (T.pack err)
  displayBuilder (DirtyTree paths) =
    "working tree is dirty (CI=true requires a clean tree); commit or stash first:\n"
      <> mconcat ["  " <> displayBuilder p <> "\n" | p <- paths]

-- | Refuse the run if the working tree has uncommitted changes. Strict mode
-- demands the SHA on the green check exactly match the bytes that were
-- tested; a dirty tree breaks that invariant by definition.
ensureCleanTree :: IO (Either ResolveError ())
ensureCleanTree = do
  (ec, out, err) <- readProcessWithExitCode gitBin ["status", "--porcelain"] ""
  pure $ case ec of
    ExitFailure n -> Left (GitStatusFailed n err)
    ExitSuccess ->
      case filter (not . T.null) (T.lines (T.pack out)) of
        [] -> Right ()
        dirty -> Left (DirtyTree dirty)

resolveSha :: IO (Either ResolveError Sha)
resolveSha = do
  (ec, out, err) <- readProcessWithExitCode gitBin ["rev-parse", "HEAD"] ""
  pure $ case ec of
    ExitSuccess -> Right $ Sha (T.strip (T.pack out))
    ExitFailure n -> Left (ResolveShaFailed n err)

resolveRepoCoords :: IO (Either ResolveError RepoCoords)
resolveRepoCoords = do
  (ec, out, err) <-
    readProcessWithExitCode
      ghBin
      ["repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"]
      ""
  pure $ case ec of
    ExitFailure n -> Left (ResolveRepoFailed n err)
    ExitSuccess ->
      case T.splitOn "/" (T.strip (T.pack out)) of
        [o, r] | not (T.null o), not (T.null r) -> Right (RepoCoords o r)
        _ -> Left (UnexpectedNameWithOwner out)

postStatus :: RepoCoords -> Sha -> Context -> CommitStatus -> IO ()
postStatus coords (Sha sha) (Context ctx) status = do
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
          "state=" <> T.unpack (wireState status),
          "-f",
          "context=" <> T.unpack ctx,
          "-f",
          "description=" <> T.unpack (wireDescription status)
        ]
  (ec, _, ghStderr) <- readProcessWithExitCode ghBin args ""
  let line = "gh: " <> T.unpack ctx <> " " <> T.unpack (wireState status)
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
