-- | Implementation of async using STM.

module AsyncSTM where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception

data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async = undefined

wait :: Async a -> IO a
wait = undefined
