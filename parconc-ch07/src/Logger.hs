-- | A simple logging service. Based on sub-section "MVar as a Simple Channel:
-- A Logging Service"

module Logger ( initLogger
              , logMessage
              , logStop
              ) where

import           Control.Concurrent

-- The MVar here is used as a way to communicate with the logger service.
-- The logger service is spawned in the `initLogger` command.
data Logger

-- | Handle to the logging service.
initLogger :: IO Logger
initLogger = undefined

-- | Log a message to the standard output.
-- Logging a message causes a side effect, so it makes sense that it returns an
-- IO ().
logMessage :: Logger -> String -> IO ()
logMessage = undefined

-- | Stop the logging process.
-- Logging must not return untill all the outstanding requests have been
-- processed.
logStop :: Logger -> IO ()
logStop = undefined
