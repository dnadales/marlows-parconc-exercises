-- | Could we implement fair MVars?

module FairMVarSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Exception
import           Data.Either
import           Data.Foldable
import           FairMVar
import           Test.Hspec

numConsumers :: Int
numConsumers = 10

numElements :: Int
numElements = 100000

elementsPerConsumer :: Int
elementsPerConsumer = numElements `quot` numConsumers

-- Value to put.
value = 15

consumeMVar :: FairMVar Int -> [Int] -> IO [Int]
consumeMVar mv buf =
  -- We return the contents of the buffer upon receiving an asynchronous
  -- exception.
  handle ((\_ -> return buf) :: (AsyncException -> IO [Int])) $ do
  v <- takeFairMVar mv
  consumeMVar mv (v:buf)

spec :: Spec
spec = do
  describe "Fair mvars" $ do
    -- TODO: This is an exercise for chapter 7
    it "behaves fairly with one consumer" $ do
      -- mv <- newEmptyMVar
      mv <- newEmptyFairMVar
      producer <- async $ sequenceA_ $ replicate numElements (putFairMVar mv value)
      consumer <- async $ sequenceA $ replicate numElements (takeFairMVar mv)
      _ <- wait producer
      xs <- wait consumer
      xs `shouldBe` replicate numElements value

    it "behaves fairly with multiple consumers" $ do
      mv <- newEmptyFairMVar
      producer <- async $ sequenceA_ $ replicate numElements (putFairMVar mv value)
      consumers <- sequenceA $ replicate numConsumers (async $ consumeMVar mv [])
      _ <- wait producer
      -- Cancel the consumers
      -- | FIXME: we should make sure that all the consumers are blocked on an
      -- empty MVar. I don't know how this can be done in a simple way. Waiting
      -- is the only idea I have.
      threadDelay (500 * (10^3))
      mapM_ cancel consumers
      results <- mapM wait consumers
      -- Check that all the elemnts have been consumed.
      sum (map length (results)) `shouldBe` numElements
      let expectedList =
            replicate numConsumers elementsPerConsumer
      -- Check that all consumers have consumed the same number of elements.
      map length (results) `shouldBe` expectedList
