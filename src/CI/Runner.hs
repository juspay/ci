{-# LANGUAGE OverloadedStrings #-}

-- | Spawn the @process-compose@ binary against an 'UpInvocation' and an
-- in-memory 'ProcessCompose' config piped on stdin. Forwards the
-- subprocess's exit code; doesn't know the flag surface, only how to
-- spawn-and-wait.
module CI.Runner (runPipeline) where

import CI.ProcessCompose (ProcessCompose, UpInvocation, processComposeBin, toUpArgs)
import qualified Data.ByteString as BS
import qualified Data.Yaml as Y
import System.Exit (ExitCode, die)
import System.IO (hClose)
import System.Process (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)

-- | Spawn @process-compose up@ from the 'UpInvocation', encode the
-- 'ProcessCompose' as YAML on stdin, and forward the subprocess's exit
-- code. 'withCreateProcess' brackets the spawn so stdin is closed and the
-- child reaped even if 'BS.hPut' throws (e.g. broken pipe).
runPipeline :: UpInvocation -> ProcessCompose -> IO ExitCode
runPipeline up pc =
  withCreateProcess cp $ \mhin _ _ ph -> case mhin of
    Nothing -> die "process-compose: stdin pipe was not created"
    Just hin -> do
      BS.hPut hin (Y.encode pc)
      hClose hin
      waitForProcess ph
  where
    cp = (proc processComposeBin (toUpArgs up)) {std_in = CreatePipe}
