-- |
module Barrier where

import           Control.Concurrent.STM

data BarrierCounter = BarrierCounter {total :: Int, current :: Int}
type Barrier = TVar BarrierCounter

-- | `newBarrier n` creates a new barrier for `n` threads.
newBarrier :: Int -> IO (Barrier)
newBarrier n = atomically $ newTVar $ BarrierCounter 0 0

-- | `barrier b` suspends the thread till all the threads are at barrier `b`.
barrierSTM :: Barrier -> STM ()
-- The code below won't work. The reason is that the STM transaction is rolled
-- back at the `retry`. Can you see this?
barrierSTM barrier = do
  b <- readTVar barrier
  let newCounter = BarrierCounter (total b) (current b + 1)
  writeTVar barrier newCounter
  if (total newCounter == current newCounter)
    then return ()
    else retry


barrier :: Barrier -> IO ()
barrier = atomically . barrierSTM
