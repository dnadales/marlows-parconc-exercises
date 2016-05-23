{-# LANGUAGE DeriveDataTypeable #-}
-- |

module MyExceptions where

import           Control.Exception hiding (bracket, finally, onException)
import           Data.Typeable

-- Implement an exception data type that is instance of `Exception`.
data MyException = MyException
  deriving (Typeable, Show)

instance Exception MyException

onException :: IO a -> IO b -> IO a
onException act what =
  -- Note that I'm using throw here. However Marlows recommend to use throwIO,
  -- since it guarantees the strict ordering w.r.t. other IO operations.
  act `catch` (\e -> do _ <- what; throw (e:: SomeException))


-- | A call of the form @bracket init final act@ performs action @init@ first,
-- then passes the result to @act@, and @act a@ gets executed, where @a@ is the
-- value contained in @init@. Action @final a@ gets always executed, where the
-- parameter to @final@ is taken from the value contained in @init@.
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket ini final act = do
  a <- ini
  c <- act a `onException` final a
  _ <- final a
  return c

-- | Executes the first action, and then the second action regardless of wether
-- the first action generated an exception.
finally :: IO a -> IO b -> IO a
finally act finalAct = bracket (return ()) (const finalAct) (const act)
