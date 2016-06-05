-- |

module Async2 where

import           Control.Concurrent
import           Control.Exception

-- Represent an asynchronous action that has been started.
data Async a = Async (MVar (Either SomeException a))


async :: IO a -> IO (Async a)
async = undefined

wait :: Async a -> IO a
wait (Async mvar) = undefined

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch = undefined
