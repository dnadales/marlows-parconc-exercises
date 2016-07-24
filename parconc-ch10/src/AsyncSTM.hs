-- | Implementation of async using STM.

module AsyncSTM where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception

data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
-- Hints: use `newEmptyTMVarIO` and `forkFinally`
async act = do
  tmv <- newEmptyTMVarIO
  tid <- forkFinally act (atomically . putTMVar tmv)
  return (Async tid tmv)

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ tmv) = readTMVar tmv

waitSTM :: Async a -> STM a
waitSTM a = do
  r <- waitCatchSTM a
  case r of
    Left e -> throwSTM e
    Right v -> return v

wait :: Async a -> IO a
wait = atomically . waitSTM

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither aa ab =
  atomically $
  (Left <$> waitSTM aa)
  `orElse`
  (Right <$> waitSTM ab)

waitAny :: [Async a] -> IO a
waitAny as =
  atomically $ foldr orElse retry $ map waitSTM as
