{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Adapt a recipe's lifecycle 'RecipeStatus' onto the GitHub commit-status
-- wire format via the @gh@ CLI. Repo coordinates and HEAD SHA are resolved
-- once at startup; 'postStatus' then posts a single REST call per
-- transition. Failures are logged to stderr and swallowed — a flaky API
-- call must not poison the recipe's own exit code.
module CI.CommitStatus
  ( -- * Wire vocabulary
    CommitStatus (..),
    toCommitStatus,

    -- * Resolved coordinates
    RepoCoords (..),
    Sha (..),
    Context (..),
    resolveRepoCoords,
    resolveSha,

    -- * Posting
    postStatus,
    buildPoster,
  )
where

import CI.Justfile (RecipeName)
import CI.RecipeStep (RecipeStatus (..))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), die)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import System.Which (staticWhich)

-- | The four GitHub commit-status states. 'Error' is reserved for the wire
-- format's out-of-band-failure slot; the recipe lifecycle never produces it
-- (see 'toCommitStatus').
data CommitStatus = Pending | Success | Failure | Error
  deriving stock (Show, Eq)

toCommitStatus :: RecipeStatus -> CommitStatus
toCommitStatus Running = Pending
toCommitStatus Succeeded = Success
toCommitStatus Failed = Failure

ghBin :: FilePath
ghBin = $(staticWhich "gh")

gitBin :: FilePath
gitBin = $(staticWhich "git")

data RepoCoords = RepoCoords {owner :: Text, repo :: Text}
  deriving stock (Show, Eq)

newtype Sha = Sha Text
  deriving newtype (Show, Eq, IsString)

newtype Context = Context Text
  deriving newtype (Show, Eq, IsString)

resolveSha :: IO Sha
resolveSha = do
  (ec, out, err) <- readProcessWithExitCode gitBin ["rev-parse", "HEAD"] ""
  case ec of
    ExitSuccess -> pure $ Sha (T.strip (T.pack out))
    ExitFailure n ->
      die $ "git rev-parse HEAD failed (" <> show n <> "): " <> err

resolveRepoCoords :: IO RepoCoords
resolveRepoCoords = do
  (ec, out, err) <-
    readProcessWithExitCode
      ghBin
      ["repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"]
      ""
  case ec of
    ExitFailure n ->
      die $ "gh repo view failed (" <> show n <> "): " <> err
    ExitSuccess ->
      case T.splitOn "/" (T.strip (T.pack out)) of
        [o, r] | not (T.null o), not (T.null r) -> pure (RepoCoords o r)
        _ -> die $ "unexpected nameWithOwner from gh: " <> out

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
  case ec of
    ExitSuccess -> pure ()
    ExitFailure n ->
      hPutStrLn stderr $
        "gh status post failed for " <> T.unpack ctx <> " (" <> show n <> "): " <> ghStderr

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

-- | Build the 'RecipeStatus' handler for a given recipe. When @CI=true@,
-- resolve repo coordinates and HEAD SHA, then return a closure that maps
-- each lifecycle event into a 'postStatus' call under the @ci/\<recipe\>@
-- context. Otherwise return a no-op so dev runs stay silent. This env-driven
-- branch is the only feature gate in the pipeline.
buildPoster :: RecipeName -> IO (RecipeStatus -> IO ())
buildPoster name = do
  enabled <- (== Just "true") <$> lookupEnv "CI"
  if not enabled
    then pure (\_ -> pure ())
    else do
      coords <- resolveRepoCoords
      sha <- resolveSha
      let ctx = Context ("ci/" <> display name)
      pure (postStatus coords sha ctx . toCommitStatus)
