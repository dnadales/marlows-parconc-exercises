-- |
module Barrier (Barrier, newBarrier, barrier) where

import           Control.Concurrent.STM

data BarrierCounter = BarrierCounter {total :: Int, current :: Int}
type Barrier = TVar BarrierCounter

-- | `newBarrier n` creates a new barrier for `n` threads.
newBarrier :: Int -> IO (Barrier)
newBarrier n = atomically $ newTVar $ BarrierCounter n 0

-- | Increments the barrier counter.
--
-- Note that we want this method to be private, since otherwise threads could
-- tamper with the barrier's invariant.
inc :: Barrier -> IO ()
inc barrier = atomically $ do
  b <- readTVar barrier
  let newCounter = BarrierCounter (total b) (current b + 1)
  writeTVar barrier newCounter

-- | `barrier b` suspends the thread till all the threads are at barrier `b`.
waitAtBarrier :: Barrier -> STM ()
waitAtBarrier barrier = do
  counter <- readTVar barrier
  if (total counter == current counter)
    then return ()
    else retry


barrier :: Barrier -> IO ()
barrier b = inc b >> (atomically $ waitAtBarrier b)
