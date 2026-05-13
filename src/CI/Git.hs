{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Git-tool operations the pipeline needs: a HEAD SHA, a clean-tree
-- precondition, and a bracketed worktree snapshot. All shell out to
-- @git@; the binary path lives here as 'gitBin'.
module CI.Git
  ( -- * Binary
    gitBin,

    -- * Values
    Sha (..),

    -- * Operations
    GitError (..),
    ensureCleanTree,
    resolveSha,
    withSnapshotWorktree,
  )
where

import CI.Subprocess (SubprocessError, runSubprocess)
import Control.Exception (SomeException, bracket_, catch)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..))
import System.Process (callProcess)
import System.Which (staticWhich)

-- | Absolute path to the @git@ binary, baked in at compile time via Nix.
gitBin :: FilePath
gitBin = $(staticWhich "git")

-- | A git commit SHA — what 'CI.CommitStatus.postStatus' attaches each
-- status check to. The constructor is exposed so consumers can pattern-match,
-- but resolution always goes through 'resolveSha'.
newtype Sha = Sha Text
  deriving newtype (Show, Eq, IsString)

-- | Failures from the git operations in this module. Subprocess failures
-- wrap 'SubprocessError'; the @binary failed (N): stderr@ formatting lives
-- in one place.
data GitError
  = GitSubprocess SubprocessError
  | -- | Working tree has uncommitted changes; carries the @git status
    -- --porcelain@ lines so the user can see what's dirty.
    DirtyTree [Text]
  deriving stock (Show)

instance Display GitError where
  displayBuilder (GitSubprocess e) = displayBuilder e
  displayBuilder (DirtyTree paths) =
    "working tree is dirty (CI=true requires a clean tree); commit or stash first:\n"
      <> mconcat ["  " <> displayBuilder p <> "\n" | p <- paths]

-- | Refuse the run if the working tree has uncommitted changes. Strict mode
-- demands the SHA on the green check exactly match the bytes that were
-- tested; a dirty tree breaks that invariant by definition.
ensureCleanTree :: IO (Either GitError ())
ensureCleanTree = do
  result <- runSubprocess "git status --porcelain" gitBin ["status", "--porcelain"] ""
  pure $ case result of
    Left e -> Left (GitSubprocess e)
    Right out -> case filter (not . T.null) (T.lines (T.pack out)) of
      [] -> Right ()
      dirty -> Left (DirtyTree dirty)

-- | Resolve the current HEAD SHA via @git rev-parse HEAD@. Used once at
-- strict-mode startup so every commit-status post against this run targets
-- the same commit.
resolveSha :: IO (Either GitError Sha)
resolveSha = do
  result <- runSubprocess "git rev-parse HEAD" gitBin ["rev-parse", "HEAD"] ""
  pure $ case result of
    Left e -> Left (GitSubprocess e)
    Right out -> Right $ Sha $ T.strip $ T.pack out

-- | Create a @git worktree@ at HEAD pinned at @snap@, run the action, and
-- remove the worktree on any exit path (success, failure, or exception,
-- including 'exitWith'). On the happy path this is one @git worktree add@
-- invocation; only if @add@ fails (typically because a previous run
-- crashed and left a stale worktree at @snap@) do we attempt a
-- @worktree remove --force@ and retry — saves a fork+exec on every
-- normal run.
--
-- Strict-mode CI runs the whole pipeline inside this worktree so that
-- (a) edits to the original working tree mid-pipeline can't leak into
-- later recipes, and (b) @git rev-parse HEAD@ from inside any recipe
-- always returns the same pinned SHA.
withSnapshotWorktree :: FilePath -> IO a -> IO a
withSnapshotWorktree snap action =
  bracket_
    (tryAdd `catch` \(_ :: SomeException) -> cleanup >> tryAdd)
    cleanup
    action
  where
    tryAdd = callProcess gitBin ["worktree", "add", "--detach", snap, "HEAD"]
    cleanup = callProcess gitBin ["worktree", "remove", "--force", snap]
