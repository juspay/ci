{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Invoke the @process-compose@ binary against an in-memory
-- 'ProcessCompose' config. The YAML is piped on stdin (@-f /dev/stdin@) so
-- there is no temp-file lifecycle to manage; the runner's exit code is the
-- pipeline's exit code.
module CI.Runner
  ( ServerMode (..),
    runPipeline,
  )
where

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

-- | Selects how process-compose's HTTP API surface is exposed (or not).
-- 'NoServer' suppresses the API entirely — used for local-mode dev runs
-- where nothing observes the state. 'UnixSocket' binds the API to a UDS at
-- the given path — used in strict mode where 'CI.Observer' subscribes to
-- the state-event stream over that socket.
data ServerMode
  = NoServer
  | UnixSocket FilePath

-- | Spawn @process-compose up@ with the encoded config on stdin, in headless
-- (no TUI) mode, and forward its exit code. 'withCreateProcess' brackets the
-- subprocess so the stdin handle is closed and the child reaped even if
-- 'BS.hPut' throws (e.g. broken pipe).
--
-- @extraArgs@ are appended verbatim after the server-mode flags, so callers
-- can override or extend process-compose's behavior (e.g. @--log-level
-- debug@) without this module knowing the flag surface.
runPipeline :: ServerMode -> [String] -> ProcessCompose -> IO ExitCode
runPipeline mode extraArgs pc =
  withCreateProcess cp $ \mhin _ _ ph -> case mhin of
    Nothing -> die "process-compose: stdin pipe was not created"
    Just hin -> do
      BS.hPut hin (Y.encode pc)
      hClose hin
      waitForProcess ph
  where
    cp =
      (proc processComposeBin (baseArgs <> serverArgs <> extraArgs))
        { std_in = CreatePipe
        }
    baseArgs = ["up", "-f", "/dev/stdin", "-t=false"]
    serverArgs = case mode of
      NoServer -> ["--no-server"]
      UnixSocket path -> ["-U", "-u", path]
