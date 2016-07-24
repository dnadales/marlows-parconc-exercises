-- |
module Barrier where

data Barrier

-- | `newBarrier n` creates a new barrier for `n` threads.
newBarrier :: Int -> IO (Barrier)
newBarrier n = undefined

-- | `barrier b` suspends the thread till all the threads are at barrier `b`.
barrier :: Barrier -> IO ()
barrier = undefined
