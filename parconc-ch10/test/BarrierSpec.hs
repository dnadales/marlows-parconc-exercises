module BarrierSpec where

import           Barrier
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Test.Hspec

inc ::  TVar Int -> IO ()
-- TODO: does strictness alters the test results?
inc tv  = atomically $ modifyTVar' tv (\x -> x + 1)

numThreads :: Int
numThreads = 100

spec :: Spec
spec = do
  describe "barrier" $ do
    it "blocks thread till all of them all at the barrier" $ do
      counter <- atomically $ newTVar 0
      br <- newBarrier numThreads
      let work = do
            inc counter
            barrier br
            atomically $ readTVar counter
      _ <- mapConcurrently (const work) [0 .. numThreads - 1]
      counter <- atomically $ readTVar counter
      counter `shouldBe` numThreads

    -- Is this the right way of handling exceptions?
    it ("throws an exeption in all other threads"
         ++ " if one of them raises an exeption") $ do
      expectationFailure "write unit tests"
