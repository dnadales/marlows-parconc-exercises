-- |

module TChanSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM   hiding (TChan, newTChan, readTChan,
                                           writeTChan)
import qualified Data.Set                 as Set
import           TChan
import           Test.Hspec

spec :: Spec
spec = do
  let testList = [0..100]

  describe "Unbounded channels" $ do
    it "buffers items" $ do
      tch <- atomically newTChan
      a <- async $ mapM_ (atomically . writeTChan tch) testList
      receivedList <- mapM (const (atomically $ readTChan tch)) testList
      wait a
      receivedList `shouldBe` testList

    it "supports multiple readers/writers" $ do
      ch <- newChan
      a0 <- async $ mapM_ (writeChan ch) (filter even testList)
      a1 <- async $ mapM_ (writeChan ch) (filter (not . even) testList)
      a2 <- async $ mapM (const (readChan ch)) (filter even testList)
      receivedList <- mapM (const (readChan ch)) (filter (not . even) testList)
      wait a0; wait a1; wait a2
      let receivedSet = Set.fromList receivedList
          testSet = Set.fromList testList
      receivedSet `shouldSatisfy` (`Set.isSubsetOf` testSet)

