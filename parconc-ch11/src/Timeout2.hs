-- |

module Timeout2 where

import           Control.Concurrent
import           Control.Concurrent.Async

-- | Extract the the value contained in the Either parameter.
extract :: Either a a -> a
extract (Left v) = v
extract (Right v) = v

timeout :: Int -> IO a -> IO (Maybe a)
timeout t io
  | t < 0 = Just <$> io
  | t == 0 = return Nothing
  | otherwise = extract <$>
                race (threadDelay t >> return Nothing) (Just <$> io)
-- This is Marlow's solution:
-- timeout n m
--     | n <  0    = fmap Just m
--     | n == 0    = return Nothing
--     | otherwise = do
--         r <- race (threadDelay n) m
--         case r of
--           Left _  -> return Nothing
--           Right a -> return (Just a)

