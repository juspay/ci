{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bracketed @git worktree@ snapshot of HEAD. Strict-mode CI runs the
-- whole pipeline inside this worktree so that (a) edits to the original
-- working tree mid-pipeline can't leak into later recipes, and (b)
-- @git rev-parse HEAD@ from inside any recipe always returns the same
-- pinned SHA — the one the user committed to test.
module CI.Snapshot (withSnapshotWorktree) where

import CI.Resolve (gitBin)
import Control.Exception (SomeException, bracket_, try)
import System.Process (callProcess)

-- | Create a @git worktree@ at HEAD pinned at @snap@, run the action, and
-- remove the worktree on any exit path (success, failure, or exception,
-- including 'exitWith'). If a previous run crashed and left a stale worktree
-- at @snap@, a best-effort @git worktree remove --force@ is attempted before
-- creation so the next run isn't blocked.
withSnapshotWorktree :: FilePath -> IO a -> IO a
withSnapshotWorktree snap action = do
  _ :: Either SomeException () <-
    try (callProcess gitBin ["worktree", "remove", "--force", snap])
  bracket_
    (callProcess gitBin ["worktree", "add", "--detach", snap, "HEAD"])
    (callProcess gitBin ["worktree", "remove", "--force", snap])
    action
