{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Adapt a recipe's lifecycle 'RecipeStatus' onto the GitHub commit-status
-- wire format via the @gh@ CLI. Repo coordinates and HEAD SHA are resolved
-- once per @run-step@ invocation (i.e. once per recipe in Phase 1, since
-- process-compose forks a fresh wrapper for each vertex); Phase 2's
-- central observer will collapse that to once per pipeline run.
-- 'postStatus' issues a single REST call per transition. Failures are
-- logged to stderr and swallowed — a flaky API call must not poison the
-- recipe's own exit code.
module CI.CommitStatus
  ( ResolveError (..),
    buildPoster,
  )
where

import CI.Justfile (RecipeName)
import CI.RecipeStep (RecipeStatus (..))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import System.Which (staticWhich)

-- | 'Error' is reserved for the wire format's out-of-band-failure slot; the
-- recipe lifecycle never produces it (see 'toCommitStatus').
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

-- | A GitHub status-check context (the unique label that groups posts of the
-- same check, shown on the PR's checks panel). Construct via 'mkContext';
-- the constructor is intentionally hidden so the @ci/\<recipe\>@ naming
-- convention lives in one place.
newtype Context = Context Text
  deriving newtype (Show, Eq, IsString)

-- | The single source of truth for status-check context names: @ci/\<recipe\>@.
mkContext :: Display a => a -> Context
mkContext name = Context ("ci/" <> display name)

-- | Failures from 'resolveSha' / 'resolveRepoCoords'. Kept as a single sum so
-- the top-level boundary ('main') has one type to display + die on, and so
-- a future caller that wants to e.g. fall back to a different coord source
-- can pattern-match without parsing strings.
data ResolveError
  = ResolveShaFailed Int String
  | ResolveRepoFailed Int String
  | UnexpectedNameWithOwner String
  deriving stock (Show)

instance Display ResolveError where
  displayBuilder (ResolveShaFailed n err) =
    "git rev-parse HEAD failed (" <> displayBuilder n <> "): " <> displayBuilder (T.pack err)
  displayBuilder (ResolveRepoFailed n err) =
    "gh repo view failed (" <> displayBuilder n <> "): " <> displayBuilder (T.pack err)
  displayBuilder (UnexpectedNameWithOwner out) =
    "unexpected nameWithOwner from gh: " <> displayBuilder (T.pack out)

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
--
-- Resolution failures are returned as 'Left' so the caller (the boundary)
-- decides how to surface them, rather than 'die'-ing deep in the call graph
-- and conflating "GitHub auth broken" with "this recipe failed".
buildPoster :: RecipeName -> IO (Either ResolveError (RecipeStatus -> IO ()))
buildPoster name = do
  enabled <- (== Just "true") <$> lookupEnv "CI"
  if not enabled
    then pure (Right (\_ -> pure ()))
    else do
      coordsE <- resolveRepoCoords
      shaE <- resolveSha
      pure $ do
        coords <- coordsE
        sha <- shaE
        Right (postStatus coords sha (mkContext name) . toCommitStatus)
