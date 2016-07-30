-- | Could we implement fair MVars?

module FairMVarSpec where

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

consumeMVar :: MVar Int -> [Int] -> IO [Int]
consumeMVar mv buf =
  -- We return the contents of the buffer upon receiving an asynchronous
  -- exception.
  handle ((\_ -> return buf) :: (AsyncException -> IO [Int])) $ do
  v <- takeMVar mv
  consumeMVar mv (v:buf)

spec :: Spec
spec = do
  describe "Fair mvars" $ do
    -- TODO: This is an exercise for chapter 7
    it "behaves fairly with one consumer" $ do
      mv <- newEmptyMVar
      producer <- async $ sequenceA_ $ replicate numElements (putMVar mv value)
      consumer <- async $ sequenceA $ replicate numElements (takeMVar mv)
      _ <- wait producer
      xs <- wait consumer
      xs `shouldBe` replicate numElements value

    it "behaves fairly with multiple consumers" $ do
      mv <- newEmptyMVar
      consumers <- sequenceA $ replicate numConsumers (async $ consumeMVar mv [])
      producer <- async $ sequenceA_ $ replicate numElements (putMVar mv value)
      _ <- wait producer
      -- Cancel the consumers
      mapM_ cancel consumers
      results <- mapM wait consumers
      -- Check that all the consumers have consumed the same number of elements
      -- (all (== elementsPerConsumer) (map length (results))) `shouldBe` True
      sum (map length (results)) `shouldBe` numElements
      let expectedList =
            replicate numConsumers elementsPerConsumer
      map length (results) `shouldBe` expectedList
