{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Subscribe to process-compose's @/process/states/ws@ WebSocket stream
-- (over a Unix domain socket) and dispatch each 'ProcessState' to a list of
-- consumers. Today's only consumer is GitHub commit-status posting; the
-- fan-out shape is deliberate so a future MCP-server consumer can be added
-- without retrofitting.
module CI.Observer
  ( Consumer,
    runObserver,
  )
where

import CI.ProcessCompose (ProcessState, ProcessStateEvent (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import qualified Network.Socket as S
import qualified Network.WebSockets as WS
import System.Directory (doesPathExist)
import System.IO (hPutStrLn, stderr)

-- | Each consumer is invoked synchronously for every event; consumers
-- should not block on slow I/O without their own forking, since the
-- observer loop processes events one at a time.
type Consumer = ProcessState -> IO ()

-- | Connect to the UDS, perform the WebSocket upgrade against
-- @/process/states/ws@, then loop decoding 'ProcessStateEvent' frames and
-- dispatching each event's 'ProcessState' to every consumer. The function
-- blocks until the WebSocket closes (i.e. process-compose exits).
--
-- 'runObserver' polls the socket path until it appears before attempting
-- the connection, so it's safe to launch concurrently with process-compose
-- without ordering the spawn against socket creation.
runObserver :: FilePath -> [Consumer] -> IO ()
runObserver sockPath consumers = do
  hPutStrLn stderr $ "observer: waiting for " <> sockPath
  waitForSocket sockPath
  hPutStrLn stderr "observer: connecting"
  sock <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
  S.connect sock (S.SockAddrUnix sockPath)
  WS.runClientWithSocket sock "localhost" "/process/states/ws" WS.defaultConnectionOptions [] $ \conn -> do
    hPutStrLn stderr "observer: connected, streaming events"
    loop conn
  where
    loop conn = do
      result <- try (WS.receiveData conn)
      case result of
        Left (e :: SomeException) ->
          hPutStrLn stderr $ "observer: stream ended (" <> show e <> ")"
        Right (bs :: BSL.ByteString) -> do
          case eitherDecodeStrict (BSL.toStrict bs) of
            Left err -> hPutStrLn stderr $ "observer: decode error: " <> err
            Right ev -> for_ consumers ($ state ev)
          loop conn

-- | Block (with a 100-attempt × 100ms backoff = ~10s ceiling) until the UDS
-- path exists on disk. Process-compose can take a couple of seconds to
-- create the socket during startup; if we still don't see it after 10s,
-- log the timeout explicitly — the next 'S.connect' call will then fail
-- with ENOENT, but at least the user sees why.
waitForSocket :: FilePath -> IO ()
waitForSocket path = go (100 :: Int)
  where
    go 0 = hPutStrLn stderr $ "observer: timed out waiting 10s for " <> path
    go n = do
      exists <- doesPathExist path
      if exists
        then pure ()
        else threadDelay 100_000 >> go (n - 1)
