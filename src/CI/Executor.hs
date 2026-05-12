{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Execute a single recipe by shelling @just --no-deps \<name\>@, streaming
-- its merged stdout+stderr through the configured 'Sinks', and returning
-- its 'ExitCode'. @--no-deps@ tells just to run only the body — our
-- scheduler has already handled the dep edges, so just must not re-trigger
-- them. Delegating to just (instead of executing the body ourselves) means
-- we inherit just's full surface: shebangs, @{{var}}@ interpolation,
-- @set shell := …@, dotenv loading, @-@ ignore-error, OS gates, etc.
module CI.Executor
  ( exec,
  )
where

import CI.Justfile (RecipeName, justBin)
import CI.Sinks (LiveTail (..), Sinks (..))
import Control.Concurrent.MVar (withMVar)
import Control.Exception (bracket, finally)
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Display (display)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode)
import System.FilePath ((<.>), (</>))
import System.IO (BufferMode (LineBuffering), Handle, IOMode (WriteMode), hClose, hIsEOF, hSetBuffering, withFile)
import System.Process
  ( CreateProcess (..),
    StdStream (NoStream, UseHandle),
    createPipe,
    proc,
    waitForProcess,
    withCreateProcess,
  )

-- | Run one recipe. Bracketed so the log handle is closed even under async
-- cancellation; the drain reaches EOF before this returns so the scheduler
-- never marks the recipe done while output is still in flight.
exec :: Sinks -> RecipeName -> IO ExitCode
exec sinks name =
  withLogHandle sinks name $ \mLog ->
    runJust sinks name mLog

-- | Open the per-recipe log file (if configured) under @logDir/<name>.log@,
-- pass its handle to the body, and close it on exit. 'withFile' provides
-- the bracketed close.
withLogHandle :: Sinks -> RecipeName -> (Maybe Handle -> IO a) -> IO a
withLogHandle sinks name k = case sinks.logDir of
  Nothing -> k Nothing
  Just dir -> do
    createDirectoryIfMissing True dir
    withFile (dir </> T.unpack (display name) <.> "log") WriteMode (k . Just)

-- | Spawn @just --no-deps \<name\>@ with stdout and stderr merged onto one
-- pipe at the OS level (single FD shared between both streams in the
-- child) so the child's own write order is preserved at the byte level.
-- One reader thread drains the pipe.
runJust :: Sinks -> RecipeName -> Maybe Handle -> IO ExitCode
runJust sinks name mLog =
  bracket createPipe closeBoth $ \(rd, wr) -> do
    let cp =
          (proc justBin ["--no-deps", T.unpack (display name)])
            { std_in = NoStream,
              std_out = UseHandle wr,
              std_err = UseHandle wr,
              close_fds = True
            }
    withCreateProcess cp $ \_ _ _ ph -> do
      hSetBuffering rd LineBuffering
      drainStream sinks name mLog rd
      waitForProcess ph
  where
    -- 'wr' is already closed by 'withCreateProcess' (the 'UseHandle'
    -- contract); the double-close is harmless because 'hClose' is
    -- idempotent.
    closeBoth (rd, wr) = hClose rd `finally` hClose wr

-- | Drain a line-oriented stream until EOF, fanning every line out to the
-- per-recipe log file and (if configured) the live tail with a name prefix.
-- The 'MVar' in 'LiveTail' serializes writes across concurrent recipes so
-- prefix lines stay coherent on the shared handle.
drainStream :: Sinks -> RecipeName -> Maybe Handle -> Handle -> IO ()
drainStream sinks name mLog h = loop
  where
    prefix = display name <> " | "
    loop = do
      eof <- hIsEOF h
      if eof
        then pure ()
        else do
          line <- TIO.hGetLine h
          for_ mLog $ \lh -> TIO.hPutStrLn lh line
          for_ sinks.liveTail $ \lt ->
            withMVar lt.liveLock $ \_ ->
              TIO.hPutStrLn lt.liveHandle (prefix <> line)
          loop
