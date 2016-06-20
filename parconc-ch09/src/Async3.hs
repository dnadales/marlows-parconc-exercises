-- |

module Async3 where

import           Control.Concurrent
import           Control.Exception

-- In this version, the `Async` data-type incorporates a thread Id so that it
-- is possible to send cancellations to it.
data Async a = Async ThreadId (MVar (Either AsyncException a))

async :: IO a -> IO (Async a)
async = undefined

cancel :: Async a -> IO ()
cancel = undefined

waitCatch :: Async a -> IO (Either AsyncException a)
waitCatch = undefined
