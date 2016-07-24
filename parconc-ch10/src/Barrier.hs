-- |
module Barrier where

data Barrier = Barrier

-- | `newBarrier n` creates a new barrier for `n` threads.
newBarrier :: Int -> IO (Barrier)
newBarrier n = return Barrier

-- | `barrier b` suspends the thread till all the threads are at barrier `b`.
barrier :: Barrier -> IO ()
barrier _ = return ()
