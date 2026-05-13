{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Bracketed @git worktree@ snapshot of HEAD. Strict-mode CI runs the
-- whole pipeline inside this worktree so that (a) edits to the original
-- working tree mid-pipeline can't leak into later recipes, and (b)
-- @git rev-parse HEAD@ from inside any recipe always returns the same
-- pinned SHA — the one the user committed to test.
--
-- Future work: a remote-build path will likely swap the @git worktree@
-- splice here for a @git archive@-over-SSH variant; the bracket signature
-- ('withSnapshotWorktree') is the seam.
module CI.Snapshot (withSnapshotWorktree) where

import Control.Exception (bracket_)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import System.Process (callProcess)
import System.Which (staticWhich)

gitBin :: FilePath
gitBin = $(staticWhich "git")

-- | Create a transient @git worktree@ at HEAD, run the action with the
-- snapshot path, and remove the worktree on any exit path (success,
-- failure, or exception, including 'exitWith'). The path lives under
-- @\$TMPDIR@ and is suffixed with the current microsecond timestamp so
-- concurrent runs don't collide.
withSnapshotWorktree :: (FilePath -> IO a) -> IO a
withSnapshotWorktree action = do
  tmpBase <- getTemporaryDirectory
  now <- getPOSIXTime
  let snap = tmpBase </> ("ci-snap-" <> show (round (now * 1e6) :: Integer))
  bracket_
    (callProcess gitBin ["worktree", "add", "--detach", snap, "HEAD"])
    (callProcess gitBin ["worktree", "remove", "--force", snap])
    (action snap)
