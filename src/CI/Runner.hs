{-# LANGUAGE OverloadedStrings #-}

-- | Invoke the @process-compose@ binary against an in-memory
-- 'ProcessCompose' config. The YAML is piped on stdin (@-f /dev/stdin@) so
-- there is no temp-file lifecycle to manage; the runner's exit code is the
-- pipeline's exit code.
module CI.Runner
  ( ServerMode (..),
    runPipeline,
  )
where

import CI.ProcessCompose (ProcessCompose, processComposeBin)
import qualified Data.ByteString as BS
import qualified Data.Yaml as Y
import System.Exit (ExitCode, die)
import System.IO (hClose)
import System.Process (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)

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
-- @logPath@ overrides process-compose's default @$TMPDIR@-rooted log file so
-- every runtime artifact lives under the caller's chosen directory.
-- @extraArgs@ are appended verbatim after the server-mode flags.
runPipeline :: ServerMode -> FilePath -> [String] -> ProcessCompose -> IO ExitCode
runPipeline mode logPath extraArgs pc =
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
    baseArgs = ["up", "-f", "/dev/stdin", "-t=false", "-L", logPath]
    serverArgs = case mode of
      NoServer -> ["--no-server"]
      UnixSocket path -> ["-U", "-u", path]
