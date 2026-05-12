{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Recursive, memoizing scheduler for a 'Plan'. Each recipe runs at most
-- once across the whole invocation, mirroring just's "a dep mentioned by
-- multiple parents only fires once" rule. Deps fan out concurrently iff the
-- parent recipe carries 'parallelDeps'; otherwise they run in declaration
-- order. Failure semantics are direction-specific: a sequential dep failure
-- short-circuits the remaining siblings, while parallel siblings all run to
-- completion and the first non-success exit code wins. Either way the
-- parent recipe's body is skipped if any dep failed.
module CI.Scheduler
  ( runPlan,
    Exec,
    PlanError (..),
  )
where

import CI.Justfile (RecipeName)
import CI.Plan (Plan, RunSpec (..))
import Control.Concurrent.Async (Async, async, cancel, mapConcurrently, wait)
import Control.Exception (Exception, finally, throwIO)
import Data.Foldable (find)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import Data.Text.Display (Display (..))
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, tryReadMVar)
import System.Exit (ExitCode (..))

-- | How to execute one recipe's body. The recipe name is enough — the
-- executor shells out to @just --no-deps \<name\>@, so it doesn't need
-- (and shouldn't see) the scheduler's 'RunSpec'.
type Exec = RecipeName -> IO ExitCode

-- | Failures detected by the scheduler before any process runs.
data PlanError = UnknownRecipe RecipeName
  deriving stock (Show)

instance Exception PlanError

instance Display PlanError where
  displayBuilder (UnknownRecipe r) =
    "scheduler: recipe " <> displayBuilder r <> " is not in the plan"

-- | Run @root@ and everything reachable from it under @plan@. Returns the
-- exit code of the run: 'ExitSuccess' iff every recipe that ran succeeded.
-- Throws 'PlanError' if @root@ (or any reachable dep) is absent from the
-- plan.
runPlan :: Exec -> Plan -> RecipeName -> IO ExitCode
runPlan execOne plan root = do
  cache <- newIORef Map.empty
  let go = scheduleNode execOne plan cache
  a <- go root
  wait a `finally` cancelAll cache

cancelAll :: IORef (Map.Map RecipeName (MVar (Async ExitCode))) -> IO ()
cancelAll cache = do
  -- Snapshot-and-clear under one atomic op so an in-flight 'scheduleNode'
  -- can't insert after we'd already iterated and skip the cancel.
  m <- atomicModifyIORef' cache (\old -> (Map.empty, old))
  -- 'tryReadMVar' rather than 'readMVar': a slot can be empty if a
  -- 'scheduleNode' caller was killed by an async exception between
  -- reserving the slot and putting the Async in. Those slots have no
  -- Async to cancel, so skip them — 'readMVar' would deadlock waiting on
  -- a put that will never happen.
  mapM_ (\mv -> tryReadMVar mv >>= mapM_ cancel) (Map.elems m)

-- | Look up (or create) the 'Async' for @name@. We reserve-slot-first to
-- avoid speculative work under the race of two concurrent first-time
-- lookups: each caller creates an empty 'MVar' and races to insert it
-- atomically. The winner — whoever's 'MVar' got into the map — spawns the
-- 'Async' and fills the slot; the loser drops its empty 'MVar' and blocks
-- on 'readMVar' over the winner's. No async is spawned speculatively.
--
-- An async exception between the atomic insert and 'putMVar' would leave
-- an empty slot visible to 'cancelAll'; that case is handled there with
-- 'tryReadMVar', not by wrapping this region in 'mask_' (which would also
-- mask the spawned 'Async' and defeat cancellation).
scheduleNode ::
  Exec ->
  Plan ->
  IORef (Map.Map RecipeName (MVar (Async ExitCode))) ->
  RecipeName ->
  IO (Async ExitCode)
scheduleNode execOne plan cache name = do
  slot <- newEmptyMVar
  claimed <- atomicModifyIORef' cache $ \m ->
    case Map.lookup name m of
      Just existing -> (m, Left existing)
      Nothing -> (Map.insert name slot m, Right slot)
  case claimed of
    Left existing -> readMVar existing
    Right mine -> do
      a <- async (executeNode execOne plan cache name)
      putMVar mine a
      pure a

-- | The body of one node's async: schedule deps (parallel or sequential per
-- the spec), short-circuit on first dep failure, then run this recipe's
-- own body. Throws 'PlanError' if a dep is absent from the plan.
executeNode ::
  Exec ->
  Plan ->
  IORef (Map.Map RecipeName (MVar (Async ExitCode))) ->
  RecipeName ->
  IO ExitCode
executeNode execOne plan cache name = do
  spec <- case Map.lookup name plan of
    Just s -> pure s
    Nothing -> throwIO (UnknownRecipe name)
  depResult <- runDeps spec
  case depResult of
    Just failed -> pure failed
    Nothing -> execOne name
  where
    go = scheduleNode execOne plan cache
    runDeps :: RunSpec -> IO (Maybe ExitCode)
    runDeps ds
      | null ds.deps = pure Nothing
      | ds.parallelDeps = do
          codes <- mapConcurrently (\d -> go d >>= wait) ds.deps
          pure (find (/= ExitSuccess) codes)
      | otherwise = sequentialDeps ds.deps
    -- Sequential: stop at first non-success so later deps don't run.
    sequentialDeps [] = pure Nothing
    sequentialDeps (d : rest) = do
      code <- go d >>= wait
      if code == ExitSuccess
        then sequentialDeps rest
        else pure (Just code)
