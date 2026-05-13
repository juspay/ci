{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Strict-mode preconditions: working-tree cleanliness, repo coordinates,
-- and HEAD SHA. Lives next to neither the reporting backend nor the
-- pipeline orchestrator because its volatility is independent: a GitHub-CI
-- platform that injects @GITHUB_SHA@/@GITHUB_REPOSITORY@ env vars changes
-- this module's resolvers without touching how commit statuses are posted,
-- and conversely a Bitbucket-pipelines backend swaps the posting layer
-- without affecting how we discover the SHA to post against.
module CI.Resolve
  ( RepoCoords (..),
    Sha (..),
    ResolveError (..),
    ensureCleanTree,
    resolveRepoCoords,
    resolveSha,
  )
where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..))
import System.Exit (ExitCode (..))
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
