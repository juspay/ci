{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bracketed @git worktree@ snapshot of HEAD. Strict-mode CI runs the
-- whole pipeline inside this worktree so that (a) edits to the original
-- working tree mid-pipeline can't leak into later recipes, and (b)
-- @git rev-parse HEAD@ from inside any recipe always returns the same
-- pinned SHA — the one the user committed to test.
module CI.Snapshot (withSnapshotWorktree) where

import CI.Resolve (gitBin)
import Control.Exception (SomeException, bracket_, catch)
import System.Process (callProcess)

-- | Create a @git worktree@ at HEAD pinned at @snap@, run the action, and
-- remove the worktree on any exit path (success, failure, or exception,
-- including 'exitWith'). On the happy path this is one @git worktree add@
-- invocation; only if @add@ fails (typically because a previous run
-- crashed and left a stale worktree at @snap@) do we attempt a
-- @worktree remove --force@ and retry — saves a fork+exec on every
-- normal run.
withSnapshotWorktree :: FilePath -> IO a -> IO a
withSnapshotWorktree snap action =
  bracket_
    (tryAdd `catch` \(_ :: SomeException) -> cleanup >> tryAdd)
    cleanup
    action
  where
    tryAdd = callProcess gitBin ["worktree", "add", "--detach", snap, "HEAD"]
    cleanup = callProcess gitBin ["worktree", "remove", "--force", snap]
