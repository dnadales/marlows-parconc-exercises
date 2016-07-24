-- |

module TChan where

import           Control.Concurrent
import           Control.Concurrent.STM hiding (TChan)

data TChan a = TChan
               (TVar (TVarList a)) -- Read end.
               (TVar (TVarList a)) -- Write end.

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)


newTChan :: STM (TChan a)
newTChan = do
  hole <- newTVar TNil
  tvr <- newTVar hole
  tvw <- newTVar hole
  return $ TChan tvr tvw

readTChan :: TChan a -> STM a
readTChan (TChan tvr _) = do
  tvxs <- readTVar tvr
  xs <- readTVar tvxs
  case xs of
    TNil -> retry
    TCons x txs -> (writeTVar tvr txs) >> return x

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _ tvw) x = do
  txs <- readTVar tvw
  hole <- newTVar TNil
  writeTVar txs (TCons x hole)
  writeTVar tvw hole



