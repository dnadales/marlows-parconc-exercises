{-# LANGUAGE DeriveDataTypeable #-}
-- |

module SwapSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Data.Typeable
import           Swap
import           Test.Hspec

data MyException = MyException deriving (Show, Typeable)
instance Exception MyException

boom :: a
boom = throw MyException

myExceptionHandler :: a -> MyException -> IO a
myExceptionHandler a _ = return a

spec :: Spec
spec = do
  describe "casMVar" $ do

    it ("sets `mvar` to `new` if the value of `mvar` equals `old` "
      ++ "and returns `True`") $ do
      m <- newMVar "foo"
      r <- casMVar m "foo" "bar"
      r `shouldBe` True
      v <- readMVar m
      v `shouldBe` "bar"

    it ("leaves the value of `mvar` unchanged if it is different from `old`"
      ++ "and returns `False`") $ do
      m <- newMVar "foo"
      r <- casMVar m "baz" "bar"
      r `shouldBe` False
      v <- readMVar m
      v `shouldBe` "foo"

    it "puts back a value on the MVar under an exception" $ do
      m <- newMVar "foo"
      a <- async $
        casMVar m boom "bar" `catch` myExceptionHandler True
      _ <- wait a
      val <- readMVar m
      val `shouldBe` "foo"
