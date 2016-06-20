-- |

module Async3Spec where

import           Async3
import           Control.Concurrent
import           Control.Exception
import           Test.Hspec

spec :: Spec
spec = do

  describe "cancel" $ do

    it "cancels the running thread with a ThreadKilled exception" $ do
      m <- newEmptyMVar
      a0 <- async $ return "ok"
      a1 <- async $ readMVar m >> return "nok"
      -- Wait for the two threads
      cancel a1
      r0 <- waitCatch a0
      r1 <- waitCatch a1
      -- Check the results
      r0 `shouldBe` (Right "ok")
      r1 `shouldBe` (Left ThreadKilled)

