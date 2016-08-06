-- |

module Timeout2Spec where

import           Control.Concurrent
import           Control.Exception
import           Data.Maybe
import           Test.Hspec
import           Timeout2

spec :: Spec
spec = do

  describe "timeout" $ do

    it "returns the result of `m` wrapped in Just when t < 0" $ do
      r <- timeout (-1) $ return "done"
      r `shouldBe` Just "done"

    it "returns Nothing when t == 0" $ do
      r <- timeout 0 $ return "done"
      r `shouldBe` Nothing

    it ("returns the result of `m` wrapped in Just"
      ++ " if `m` completes before the timeout") $ do
      r <- timeout 100 $ return "done"
      r `shouldBe` Just "done"

    it ("cancels the computation and returns `Nothing` "
        ++ "if `m` does not complete before the timeout") $ do
      r <- timeout 1000 $ do threadDelay 2000; return "done"
      r `shouldBe` Nothing

    it "can be nested" $ do
      r <- timeout 100 $ do
        r' <- timeout 200 $ return "done"
        return $ fromMaybe "not done!" r'
      r `shouldBe` Just "done"

    it "cancellation works under nesting" $ do
      r <- timeout 1000 $ do
        r' <- timeout 2000 $ do threadDelay 3000; return "done"
        return $ fromMaybe "not done!" r'
      r `shouldBe` Nothing

    it "cancellation works under nesting (variant 2)" $ do
      r <- timeout 2000 $ do
        r' <- timeout 1000 $ do threadDelay 3000; return "done"
        return $ fromMaybe "not done!" r'
      r `shouldBe` Just "not done!"

    it "cancellation works under nesting (variant 3)" $ do
      r <- timeout 2000 $ do
        r' <- timeout 1000 $ do
          r'' <- timeout 3000 $ do
            threadDelay 4000; return "done"
          return $ fromMaybe "not done!" r''
        return $ fromMaybe "not done!" r'
      r `shouldBe` Just "not done!"

  -- Note that this test will fail (both for Marlow's and my solution). Running
  -- in the context of the current thread seemed to be a property of the
  -- previous implementation of timeout (Chapter 9).
  --
  -- The above is clarified in page 202 of Marlows' book.
    it "runs m in the context of the current thread" $ do
      tid <- myThreadId
      r <- timeout 100 $ do tid' <- myThreadId; return (tid == tid')
      r `shouldBe` Just True

