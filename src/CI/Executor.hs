{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Execute a single 'RunSpec' body: shell @sh -c@, stream its merged
-- stdout+stderr through the configured 'Sinks', return its 'ExitCode'.
-- Bodies are joined into one shell script with @set -e@ so a failing line
-- aborts the recipe — matching just's per-line failure semantics.
module CI.Executor
  ( exec,
  )
where

import CI.Justfile (RecipeName)
import CI.Plan (ExecSpec (..))
import CI.Sinks (LiveTail (..), Sinks (..))
import Control.Concurrent.MVar (withMVar)
import Control.Exception (bracket, finally)
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Display (display)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
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

-- | Run a recipe's body. Pure dep-aggregators (empty 'bodyLines') return
-- 'ExitSuccess' immediately without spawning a shell.
exec :: Sinks -> RecipeName -> ExecSpec -> IO ExitCode
exec _ _ es | null es.bodyLines = pure ExitSuccess
exec sinks name es =
  withLogHandle sinks name $ \mLog ->
    runScript sinks name mLog (renderScript es.bodyLines)

-- | Join body lines into one shell script. @set -e@ aborts on first failure,
-- mirroring just's @sh@ default.
renderScript :: [T.Text] -> T.Text
renderScript ls = T.unlines ("set -e" : ls)

-- | Open the per-recipe log file (if configured) under @logDir/<name>.log@,
-- pass its handle to the body, and close it on exit — even under async
-- cancellation. 'withFile' provides the bracketed close.
withLogHandle :: Sinks -> RecipeName -> (Maybe Handle -> IO a) -> IO a
withLogHandle sinks name k = case sinks.logDir of
  Nothing -> k Nothing
  Just dir -> do
    createDirectoryIfMissing True dir
    withFile (dir </> T.unpack (display name) <.> "log") WriteMode (k . Just)

-- | Spawn @sh -c <script>@ with stdout and stderr merged onto one pipe at
-- the OS level (single FD shared between both streams in the child) so the
-- child's own write order is preserved at the byte level. One reader thread
-- drains the pipe; the scheduler only sees this recipe complete after the
-- drain reaches EOF, so downstream recipes never start before predecessor
-- logs have flushed.
runScript :: Sinks -> RecipeName -> Maybe Handle -> T.Text -> IO ExitCode
runScript sinks name mLog script =
  bracket createPipe closeBoth $ \(rd, wr) -> do
    let cp =
          (proc "/bin/sh" ["-c", T.unpack script])
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
