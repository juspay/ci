{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Per-recipe lifecycle wrapper. Each process-compose vertex invokes this
-- via the @run-step@ subcommand; the wrapper emits 'Running', spawns
-- @just --no-deps <name>@ with inherited stdio (so its output streams through
-- process-compose's logger), then emits the terminal 'RecipeStatus' derived
-- from the recipe's exit code.
--
-- The reporting side-effect is injected as a 'RecipeStatus'-consuming
-- callback so this module owns the lifecycle vocabulary alone and does not
-- depend on any specific reporting backend. Dev runs pass a no-op handler;
-- CI runs pass one that ultimately calls @gh api@.
--
-- Phase 1 only: a recipe that is never spawned (because its dep failed) gets
-- no terminal status. Phase 2 will close that gap, at which point this
-- module is expected to be replaced by a central observer subscribing to
-- process-compose's @/process/states/ws@ stream.
module CI.RecipeStep
  ( RecipeStatus (..),
    runStep,
  )
where

import CI.Justfile (RecipeName, justBin)
import qualified Data.Text as T
import Data.Text.Display (display)
import System.Exit (ExitCode (..))
import System.Process (proc, waitForProcess, withCreateProcess)

-- | The lifecycle does not model an out-of-band "error" (infra failure) —
-- that vocabulary belongs to the reporter layer if anywhere.
data RecipeStatus = Running | Succeeded | Failed
  deriving stock (Show, Eq)

toRecipeStatus :: ExitCode -> RecipeStatus
toRecipeStatus ExitSuccess = Succeeded
toRecipeStatus (ExitFailure _) = Failed

-- | Execute one recipe with lifecycle reporting. @onStatus@ is invoked with
-- 'Running' before the recipe spawns and with the terminal 'RecipeStatus'
-- (derived from the exit code) after it finishes. The recipe's own exit
-- code is returned so the caller (typically @main@'s @run-step@ branch)
-- can propagate it via 'exitWith'.
runStep :: (RecipeStatus -> IO ()) -> RecipeName -> IO ExitCode
runStep onStatus name = do
  onStatus Running
  exit <- spawnRecipe name
  onStatus (toRecipeStatus exit)
  pure exit

spawnRecipe :: RecipeName -> IO ExitCode
spawnRecipe name =
  withCreateProcess (proc justBin ["--no-deps", T.unpack (display name)]) $ \_ _ _ ph ->
    waitForProcess ph
