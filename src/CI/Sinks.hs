{-# LANGUAGE OverloadedStrings #-}

-- | Output sinks for the runner. Per-recipe log files and a prefixed live tail
-- are independent: either or both can be enabled. Concurrent writers to the
-- live-tail 'Handle' are serialized through an 'MVar' so prefix lines stay
-- coherent under fanout.
module CI.Sinks
  ( Sinks (..),
    LiveTail (..),
    newLiveTail,
    noSinks,
  )
where

import Control.Concurrent.MVar (MVar, newMVar)
import System.IO (Handle)

-- | A handle paired with its serialization lock. Constructed via 'newLiveTail'
-- so callers can't accidentally share a handle without sharing the lock.
data LiveTail = LiveTail
  { liveLock :: MVar (),
    liveHandle :: Handle
  }

-- | Wire one 'Handle' (typically 'stderr') as a live tail with a fresh lock.
newLiveTail :: Handle -> IO LiveTail
newLiveTail h = do
  lock <- newMVar ()
  pure (LiveTail lock h)

-- | Where each recipe's output goes.
data Sinks = Sinks
  { -- | If 'Just', the directory under which @<recipe>.log@ files are written.
    logDir :: Maybe FilePath,
    -- | If 'Just', every output line is also echoed to this handle, prefixed
    -- with the recipe name. Multiple recipes share the handle; the lock
    -- keeps lines from interleaving.
    liveTail :: Maybe LiveTail
  }

-- | No sinks configured: discard output. Useful for tests.
noSinks :: Sinks
noSinks = Sinks Nothing Nothing
