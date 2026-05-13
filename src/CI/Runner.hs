{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Invoke the @process-compose@ binary against an in-memory
-- 'ProcessCompose' config. The YAML is piped on stdin (@-f /dev/stdin@) so
-- there is no temp-file lifecycle to manage; the runner's exit code is the
-- pipeline's exit code.
module CI.Runner (runPipeline) where

import CI.ProcessCompose (ProcessCompose)
import qualified Data.ByteString as BS
import qualified Data.Yaml as Y
import System.Exit (ExitCode, die)
import System.IO (hClose)
import System.Process (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)
import System.Which (staticWhich)

-- | Absolute path to the @process-compose@ binary, baked in at compile time
-- via Nix (see @settings.ci.extraBuildTools@ in @flake.nix@).
processComposeBin :: FilePath
processComposeBin = $(staticWhich "process-compose")

-- | Spawn @process-compose up@ with the encoded config on stdin, in headless
-- mode (TUI and HTTP server both disabled), and forward its exit code.
-- 'withCreateProcess' brackets the subprocess so the stdin handle is closed
-- and the child reaped even if 'BS.hPut' throws (e.g. broken pipe).
runPipeline :: ProcessCompose -> IO ExitCode
runPipeline pc =
  withCreateProcess cp $ \mhin _ _ ph -> case mhin of
    Nothing -> die "process-compose: stdin pipe was not created"
    Just hin -> do
      BS.hPut hin (Y.encode pc)
      hClose hin
      waitForProcess ph
  where
    cp =
      (proc processComposeBin ["up", "-f", "/dev/stdin", "-t=false", "--no-server"])
        { std_in = CreatePipe
        }
