-- | Have async implementing the functor class.

module AsyncFunctor where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception

-- | What if we wanted to implemement Async as an instance of functor?
--
-- Async was defined as:
--
--     data Async a = Async ThreadId (TMVar (Either SomeException a))
--
-- If we wanted to implement fmap with this representation we'd soon see that
-- we'll run into trouble:
--
-- fmap :: (a -> b) -> Async a -> Async b
-- fmap f (Async tid tmv) =
--
-- The only way to extract the value of tmv to apply f seems to be via an IO
-- action. But once in the IO monad we cannot go back. Hence, we require a
-- modification of the STM data type.
--
data Async a = Async ThreadId (STM (Either SomeException a))

instance Functor Async where
  -- fmap :: (a -> b) -> Async a -> Async b
  fmap f (Async tid stm) =
    Async tid (g <$> stm)
    where g (Right val) = Right (f val)
          g (Left e) = (Left e)

async :: IO a -> IO (Async a)
async act = do
  tmv <- newEmptyTMVarIO
  tid <- forkFinally act (atomically . putTMVar tmv)
  return (Async tid (readTMVar tmv))


waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ tmv) = tmv

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
