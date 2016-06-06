-- |

module Async2Spec where

import           Async2
import           Control.Concurrent
import           Control.Exception  hiding (bracket, finally, onException)
import           Data.Either
import           MyExceptions
import           Test.Hspec

boom :: IO a
boom = throw $ MyException

myException :: Selector MyException
myException = const True

spec :: Spec
spec = do

  describe "async" $ do

    it "performs actions asynchronously and allows to wait for their results" $ do
      let str0 = "hello "
          str1 = " async world"
      a0 <- async $ return str0
      a1 <- async $ return str1
      r0 <- wait a0
      r1 <- wait a1
      r0 ++ r1 `shouldBe` str0 ++ str1

    it "propagates exceptions to the waiting thread" $ do
      (do
          a <- async $ boom
          wait a
        )
      `shouldThrow` myException

  describe "waitCatch" $ do

    it "passes the exception to the waiting thread" $ do
      a <- async $ boom
      r <- waitCatch a
      let exceptionStr = either show (const "") r
      exceptionStr `shouldBe` show MyException


