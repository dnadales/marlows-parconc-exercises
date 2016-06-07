-- |

module Async2 where

import           Control.Concurrent
import           Control.Exception

-- Represent an asynchronous action that has been started.
data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async act = do
  mvar <- newEmptyMVar
  _ <- forkIO $ do
    result <- try act
    putMVar mvar result
  return (Async mvar)

wait :: Async a -> IO a
wait (Async mvar) = do
  result <- readMVar mvar
  case result of
    Left e -> throw e
    Right a -> return a

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async mvar) = readMVar mvar
