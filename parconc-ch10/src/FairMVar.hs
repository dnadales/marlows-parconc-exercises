-- | Could we implement fair MVars?

module FairMVar where

import           Control.Concurrent
import           Control.Concurrent.STM

data FairMVar a = FairMVar { getValue :: TVar (Maybe a)
                           , getQueue :: TVar [ThreadId]}

newEmptyFairMVar :: IO (FairMVar a)
newEmptyFairMVar = do
  tv <- atomically $ newTVar Nothing
  tq <- atomically $ newTVar []
  return (FairMVar tv tq)

takeFairMVar :: FairMVar a -> IO a
takeFairMVar (FairMVar tv tq) = do
  mTid <- myThreadId
  atomically $ do
    qs <- readTVar tq
    writeTVar tq (qs ++ [mTid])
  atomically $ do
    qs <- readTVar tq
    -- Invariant: the queue always have at least one element
    if (head qs == mTid) then
      do mv <- readTVar tv
         case mv of
           Nothing -> retry
           Just v -> do
             writeTVar tv Nothing
             writeTVar tq (tail qs)
             return v
    else retry

putFairMVar :: FairMVar a -> a -> IO ()
putFairMVar (FairMVar tv tq) x = atomically $ do
  mv <- readTVar tv
  case mv of
    Nothing -> writeTVar tv (Just x)
    _ -> retry
