-- |

module Async3 where

import           Control.Concurrent
import           Control.Exception

-- In this version, the `Async` data-type incorporates a thread Id so that it
-- is possible to send cancellations to it.
data Async a = Async ThreadId (MVar (Either AsyncException a))

async :: IO a -> IO (Async a)
-- The code below is wrong! If you mask after the fork, the exception might arrive and
-- you will never be able to put the MVar!

-- async act = do
--   mvar <- newEmptyMVar
--   tId <- forkIO $ mask $ \restore -> do r <- restore (try act); putMVar mvar r
--   return $ Async tId mvar

-- That's why you need to put it in the same thread that is calling `async`:
async action = do
   m <- newEmptyMVar
   t <- mask $ \restore ->
     forkIO (do r <- try (restore action); putMVar m r)
   return (Async t m)

cancel :: Async a -> IO ()
cancel (Async tId _) = throwTo tId ThreadKilled

waitCatch :: Async a -> IO (Either AsyncException a)
waitCatch (Async _ mvar) = readMVar mvar
