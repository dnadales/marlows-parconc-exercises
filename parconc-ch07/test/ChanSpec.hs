-- |

module ChanSpec where

import           Chan
import           Control.Concurrent       (forkIO, yield)
import           Control.Concurrent.Async
import           Control.Exception
import qualified Data.Set                 as Set
import           Test.Hspec

deadlockException :: Selector BlockedIndefinitelyOnMVar
deadlockException = const True

spec :: Spec
spec = do
  let testList = [0..100]

  describe "Unbounded channels" $ do

    it "buffers items" $ do
      ch <- newChan
      a <- async $ mapM_ (writeChan ch) testList
      receivedList <- mapM (const (readChan ch)) testList
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

  describe "Multicast channels" $ do

    it "allows two processes to read the same items" $ do
      ch <- newChan
      chDup <- dupChan ch
      a0 <- async $ mapM_ (writeChan ch)  testList
      a1 <- async $ mapM (const (readChan ch)) testList
      receivedList <- mapM (const (readChan chDup)) testList
      wait a0; wait a1
      receivedList `shouldBe` testList

  describe "unGetChan" $ do

    it "puts back a values at the front of the channel" $ do
      let val = 33
      ch <- newChan
      a0 <- async $ mapM_ (writeChan ch) [val, val]
      rVal <- readChan ch
      wait a0
      unGetChan ch val
      rVal `shouldBe` val
      --expectationFailure "TODO: write this unit test"
      rVal2 <- readChan ch
      rVal2 `shouldBe` val

    -- There is no known implementation of 'unGetChan' based on the
    -- representation of Chan given in chapter 7 that avoids this problem.
    it "deadlocks on an empty channel when other process is waiting\
\       (see page 139 for further explanation)" $
      (do
          let val = 10
          ch <- newChan :: IO (Chan Int)
          forkIO $ do readChan ch; return ()
          -- Yield the control to the other thread to allow `readChan` to occur
          -- first.
          yield
          unGetChan ch val
      )
      `shouldThrow` deadlockException
