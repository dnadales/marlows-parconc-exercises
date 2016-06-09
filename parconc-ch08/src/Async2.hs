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

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither la ra = do
  mvar <- newEmptyMVar
  -- Note that for the left part, in case of successful completion, mvar will
  -- contain: Right (Left someResult)
  forkIO $ do r <- try (fmap Left (wait la)); putMVar mvar r
  forkIO $ do r <- try (fmap Right (wait ra)); putMVar mvar r
  wait (Async mvar)

waitAny :: [Async a] -> IO a
waitAny xs = do
  mvar <- newEmptyMVar
  mapM_ (\a -> forkIO $ do r <- try (wait a); putMVar mvar r) xs
  wait (Async mvar)
