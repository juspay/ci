-- | Fan-out dispatch over process-compose's state-event stream. The
-- subscription mechanism (UDS+WS+JSON decode) lives in
-- "CI.ProcessCompose" alongside everything else process-compose; this
-- module owns only the @[Consumer]@ shape — one event arrives, every
-- consumer runs against it. Today's only consumer is GitHub
-- commit-status posting; the list shape is deliberate so a future
-- MCP-server consumer or on-disk event logger can be added without
-- retrofitting.
module CI.Observer
  ( Consumer,
    runObserver,
  )
where

import CI.ProcessCompose (ProcessState, subscribeStates)
import Data.Foldable (for_)

-- | One callback per 'ProcessState' event. Invoked synchronously in the
-- subscription loop; consumers that need to do slow work should fork
-- internally.
type Consumer = ProcessState -> IO ()

-- | Subscribe to process-compose at @sockPath@ and dispatch each
-- 'ProcessState' to every consumer. Blocks until the WebSocket closes
-- (process-compose exits).
runObserver :: FilePath -> [Consumer] -> IO ()
runObserver sockPath consumers =
  subscribeStates sockPath $ \ps -> for_ consumers ($ ps)
