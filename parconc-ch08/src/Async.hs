-- |

module Async where

import           Control.Concurrent

-- Represent an asynchronous action that has been started.
data Async a

async :: IO a -> IO (Async a)
async = undefined

wait :: Async a -> IO a
wait = undefined
