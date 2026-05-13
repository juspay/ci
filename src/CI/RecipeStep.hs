{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Per-recipe lifecycle wrapper. Each process-compose vertex invokes this
-- via the @run-step@ subcommand; the wrapper posts a "started" signal, runs
-- @just --no-deps <name>@ with inherited stdio (so its output streams through
-- process-compose's logger), then posts a terminal signal carrying the
-- recipe's exit code.
--
-- The reporting side-effect is injected as a 'CommitStatus'-consuming
-- callback so the module does not depend on 'CI.GitHubStatus'. Dev runs
-- pass a no-op poster; CI runs pass one that calls @gh api@.
--
-- Phase 1 only: a recipe that is never spawned (because its dep failed) gets
-- no terminal status. Phase 2 will close that gap, at which point this
-- module is expected to be replaced by a central observer subscribing to
-- process-compose's @/process/states/ws@ stream.
module CI.RecipeStep
  ( CommitStatus (..),
    toCommitStatus,
    runStep,
  )
where

import CI.Justfile (RecipeName)
import qualified Data.Text as T
import Data.Text.Display (display)
import System.Exit (ExitCode (..))
import System.Process (proc, waitForProcess, withCreateProcess)
import System.Which (staticWhich)

justBin :: FilePath
justBin = $(staticWhich "just")

-- | The four GitHub commit-status states. 'Error' is included for fidelity
-- with the wire format even though 'toCommitStatus' never produces it —
-- recipe exit codes only distinguish success from failure.
data CommitStatus = Pending | Success | Failure | Error
  deriving stock (Show, Eq)

toCommitStatus :: ExitCode -> CommitStatus
toCommitStatus ExitSuccess = Success
toCommitStatus (ExitFailure _) = Failure

runStep :: (CommitStatus -> IO ()) -> RecipeName -> IO ExitCode
runStep postStatus name = do
  postStatus Pending
  exit <- spawnRecipe name
  postStatus (toCommitStatus exit)
  pure exit

spawnRecipe :: RecipeName -> IO ExitCode
spawnRecipe name =
  withCreateProcess (proc justBin ["--no-deps", T.unpack (display name)]) $ \_ _ _ ph ->
    waitForProcess ph
