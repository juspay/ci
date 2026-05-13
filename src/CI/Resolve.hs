{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Strict-mode preconditions: working-tree cleanliness, repo coordinates,
-- and HEAD SHA. Separated from the reporting backend so the resolvers
-- (what @git@ / @gh@ tell us about the checkout) can evolve independently
-- of how a status post is shaped on the wire.
module CI.Resolve
  ( RepoCoords (..),
    Sha (..),
    ResolveError,
    ensureCleanTree,
    resolveRepoCoords,
    resolveSha,
    gitBin,
  )
where

import CI.Subprocess (SubprocessError, runSubprocess)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..))
import System.Which (staticWhich)

ghBin :: FilePath
ghBin = $(staticWhich "gh")

-- | Absolute path to the @git@ binary, baked in at compile time via Nix.
-- Re-exported so other modules ("CI.Snapshot") that need to shell out to
-- @git@ don't each splice their own.
gitBin :: FilePath
gitBin = $(staticWhich "git")

-- | The two halves of a GitHub repository identifier, as in
-- @\<owner\>/\<repo\>@. Resolved once from @gh repo view@ and threaded into
-- 'CI.CommitStatus.postStatus' alongside the 'Sha'.
data RepoCoords = RepoCoords {owner :: Text, repo :: Text}
  deriving stock (Show, Eq)

-- | A git commit SHA — what 'CI.CommitStatus.postStatus' attaches each
-- status check to. The constructor is exposed so consumers can pattern-match,
-- but resolution always goes through 'resolveSha'.
newtype Sha = Sha Text
  deriving newtype (Show, Eq, IsString)

-- | Failures from the resolve / clean-tree checks performed at the top of
-- strict mode. Subprocess failures wrap 'SubprocessError' so the
-- @binary failed (N): stderr@ formatting lives in one place.
data ResolveError
  = ResolveShaFailed SubprocessError
  | ResolveRepoFailed SubprocessError
  | UnexpectedNameWithOwner String
  | GitStatusFailed SubprocessError
  | -- | Working tree has uncommitted changes; carries the @git status
    -- --porcelain@ lines so the user can see what's dirty.
    DirtyTree [Text]
  deriving stock (Show)

instance Display ResolveError where
  displayBuilder (ResolveShaFailed e) = displayBuilder e
  displayBuilder (ResolveRepoFailed e) = displayBuilder e
  displayBuilder (GitStatusFailed e) = displayBuilder e
  displayBuilder (UnexpectedNameWithOwner out) =
    "unexpected nameWithOwner from gh: " <> displayBuilder (T.pack out)
  displayBuilder (DirtyTree paths) =
    "working tree is dirty (CI=true requires a clean tree); commit or stash first:\n"
      <> mconcat ["  " <> displayBuilder p <> "\n" | p <- paths]

-- | Refuse the run if the working tree has uncommitted changes. Strict mode
-- demands the SHA on the green check exactly match the bytes that were
-- tested; a dirty tree breaks that invariant by definition.
ensureCleanTree :: IO (Either ResolveError ())
ensureCleanTree = do
  result <- runSubprocess "git status --porcelain" gitBin ["status", "--porcelain"] ""
  pure $ case result of
    Left e -> Left (GitStatusFailed e)
    Right out -> case filter (not . T.null) (T.lines (T.pack out)) of
      [] -> Right ()
      dirty -> Left (DirtyTree dirty)

-- | Resolve the current HEAD SHA via @git rev-parse HEAD@. Used once at
-- strict-mode startup so every commit-status post against this run targets
-- the same commit.
resolveSha :: IO (Either ResolveError Sha)
resolveSha = do
  result <- runSubprocess "git rev-parse HEAD" gitBin ["rev-parse", "HEAD"] ""
  pure $ case result of
    Left e -> Left (ResolveShaFailed e)
    Right out -> Right $ Sha $ T.strip $ T.pack out

-- | Resolve the @\<owner\>/\<repo\>@ this checkout reports to via
-- @gh repo view --json nameWithOwner@. Falls out as 'RepoCoords' so callers
-- never see a slash-separated string.
resolveRepoCoords :: IO (Either ResolveError RepoCoords)
resolveRepoCoords = do
  result <-
    runSubprocess
      "gh repo view"
      ghBin
      ["repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"]
      ""
  pure $ case result of
    Left e -> Left (ResolveRepoFailed e)
    Right out -> case T.splitOn "/" (T.strip (T.pack out)) of
      [o, r] | not (T.null o), not (T.null r) -> Right (RepoCoords o r)
      _ -> Left (UnexpectedNameWithOwner out)
