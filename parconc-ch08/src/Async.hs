-- |

module Async where

import           Control.Concurrent

-- Represent an asynchronous action that has been started.
data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async act = do
  mvar <- newEmptyMVar
  forkIO $ do result <- act; putMVar mvar result
  return (Async mvar)

wait :: Async a -> IO a
wait (Async mvar) =
  -- My first attempt was:
  --
  -- takeMVar mvar
  --
  -- This allowed me to pass the tests, but using readMVar allows multiple
  -- processes to wait for the same action.
  readMVar mvar
