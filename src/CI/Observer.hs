{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Subscribe to process-compose's @/process/states/ws@ WebSocket stream
-- (over a Unix domain socket) and dispatch each 'ProcessStateEvent' to a
-- list of consumers. Today's only consumer is GitHub commit-status posting;
-- the fan-out shape is deliberate so a future MCP-server consumer can be
-- added without retrofitting.
module CI.Observer
  ( -- * Wire format
    ProcessStateEvent (..),
    ProcessState (..),

    -- * Running the observer
    Consumer,
    runObserver,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson (FromJSON, eitherDecodeStrict)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Network.Socket as S
import qualified Network.WebSockets as WS
import System.Directory (doesPathExist)
import System.IO (hPutStrLn, stderr)

-- | Mirrors process-compose's @ProcessStateEvent@ wire type
-- (@src/types/process.go:282@). Only the fields the observer reads are
-- decoded; aeson ignores the rest.
data ProcessStateEvent = ProcessStateEvent
  { -- | @true@ for events from the initial replay on connect, @omitempty@
    -- (i.e. absent) for live transitions — modelled as @Maybe Bool@ and
    -- defaulted to 'False' via 'isSnapshot'.
    snapshot :: Maybe Bool,
    state :: ProcessState
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

isSnapshot :: ProcessStateEvent -> Bool
isSnapshot = fromMaybe False . snapshot

-- | Subset of process-compose's @ProcessState@ this observer cares about.
data ProcessState = ProcessState
  { name :: Text,
    status :: Text,
    exit_code :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | Each consumer is invoked synchronously for every (non-snapshot) event;
-- consumers should not block on slow I/O without their own forking, since
-- the observer loop processes events one at a time.
type Consumer = ProcessStateEvent -> IO ()

-- | Connect to the UDS, perform the WebSocket upgrade against
-- @/process/states/ws@, then loop forever decoding 'ProcessStateEvent'
-- JSON frames and dispatching each to every consumer. The function blocks
-- until the WebSocket closes (i.e. process-compose exits).
--
-- 'runObserver' polls the socket path until it appears before attempting
-- the connection, so it's safe to launch concurrently with process-compose
-- without ordering the spawn against socket creation.
runObserver :: FilePath -> [Consumer] -> IO ()
runObserver sockPath consumers = do
  waitForSocket sockPath
  sock <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
  S.connect sock (S.SockAddrUnix sockPath)
  WS.runClientWithSocket sock "localhost" "/process/states/ws" WS.defaultConnectionOptions [] $ \conn ->
    forever $ do
      result <- try (WS.receiveData conn)
      case result of
        Left (e :: SomeException) -> do
          hPutStrLn stderr $ "observer: connection closed (" <> show e <> ")"
          WS.sendClose conn ("bye" :: Text)
          fail "observer terminated"
        Right (bs :: BSL.ByteString) ->
          case eitherDecodeStrict (BSL.toStrict bs) of
            Left err -> hPutStrLn stderr $ "observer: decode error: " <> err
            Right ev
              | isSnapshot ev -> pure ()
              | otherwise -> for_ consumers ($ ev)

-- | Block (with a 30-attempt × 100ms backoff = ~3s ceiling) until the UDS
-- path exists on disk. Process-compose creates the socket synchronously
-- during startup, so 3s is generous; if we still don't see it, the next
-- 'S.connect' call will fail loudly.
waitForSocket :: FilePath -> IO ()
waitForSocket path = go (30 :: Int)
  where
    go 0 = pure ()
    go n = do
      exists <- doesPathExist path
      if exists
        then pure ()
        else threadDelay 100_000 >> go (n - 1)
