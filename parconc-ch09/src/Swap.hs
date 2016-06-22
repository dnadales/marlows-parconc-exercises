-- |

module Swap where

import           Control.Concurrent
import           Control.Exception

-- Compare and swap. `casMvar mvar old new` inspects the content of `mvar`. If
-- it equals `old`, then it is replaced by new, and it returns `True`. Otherwise
-- it is left unmodified and it returns `False`.
casMVar :: Eq a => MVar a -> a -> a -> IO Bool

-- This implementation is not correct since an implementation can arise after
-- taking the value of `mvar` and before puting a value back.
-- casMVar mvar old new =  do
--   cur <- takeMVar mvar
--   if cur == old
--   then putMVar mvar new >> return True
--   else putMVar mvar cur >> return False

casMVar mvar old new =  mask $ \restore -> do
  cur <- takeMVar mvar
  (val, result) <- restore (eval cur)
                   `catch` \e -> do (putMVar mvar cur); throw (e :: SomeException)
  putMVar mvar val
  return result
  where eval cur =
          if cur == old
          then return (new, True)
          else return (cur, False)

-- Alternativelly the same can be achieved by using `modifyMVar`
-- casMVar mvar old new = modifyMVar mvar $ \cur ->
--   if cur == old
--           then return (new, True)
--           else return (cur, False)
