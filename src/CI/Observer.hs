{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Subscribe to process-compose's @/process/states/ws@ WebSocket stream
-- (over a Unix domain socket) and dispatch each 'ProcessState' to a list of
-- consumers. Today's only consumer is GitHub commit-status posting; the
-- fan-out shape is deliberate so a future MCP-server consumer can be added
-- without retrofitting.
module CI.Observer
  ( -- * Wire format
    ProcessState (..),
    ProcessStatus (..),

    -- * Running the observer
    Consumer,
    runObserver,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict, withText)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Network.Socket as S
import qualified Network.WebSockets as WS
import System.Directory (doesPathExist)
import System.IO (hPutStrLn, stderr)

-- | The four process-compose states that map onto a GitHub commit status.
-- Everything else (Pending, Launching, Restarting, …) lands in 'PsOther'
-- and is silently dropped by consumers that only care about the four.
-- Typed (rather than left as a raw 'Text') so a typo or an upstream rename
-- becomes a pattern-match exhaustiveness warning instead of a silent miss.
data ProcessStatus
  = PsRunning
  | PsCompleted
  | PsSkipped
  | PsErrored
  | PsOther Text
  deriving stock (Show, Eq)

instance FromJSON ProcessStatus where
  parseJSON = withText "ProcessStatus" $ \t -> pure $ case t of
    "Running" -> PsRunning
    "Completed" -> PsCompleted
    "Skipped" -> PsSkipped
    "Error" -> PsErrored
    other -> PsOther other

-- | Subset of process-compose's @ProcessState@ this observer cares about.
data ProcessState = ProcessState
  { name :: Text,
    status :: ProcessStatus,
    exit_code :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | Mirrors process-compose's @ProcessStateEvent@ wire type. The
-- @snapshot@ field (true on initial replay, omitted on live transitions)
-- is decoded but stripped before consumers see anything — the observer
-- treats both kinds identically, since posting "pending" for a recipe
-- that's already running is idempotent on GitHub's side, and processing
-- snapshot frames catches recipes whose @Running@ event preceded our
-- connection.
data ProcessStateEvent = ProcessStateEvent
  { snapshot :: Maybe Bool,
    state :: ProcessState
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

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
-- the next 'S.connect' call will fail loudly.
waitForSocket :: FilePath -> IO ()
waitForSocket path = go (100 :: Int)
  where
    go 0 = pure ()
    go n = do
      exists <- doesPathExist path
      if exists
        then pure ()
        else threadDelay 100_000 >> go (n - 1)
