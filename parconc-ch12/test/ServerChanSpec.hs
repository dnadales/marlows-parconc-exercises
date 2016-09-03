-- |

module ServerChanSpec where

import           Client                   (waitForNConnectedClients,
                                           waitForServer)
import           Control.Concurrent       (forkIO, killThread, threadDelay)
import           Control.Concurrent.Async (async, wait)
import           Horde                    (changeTwice, spawnChanger,
                                           spawnListeners)
import           Message                  (changes, results)
import           ServerChan               (serve)
import           Test.Hspec

spec :: Spec
spec = do
  describe "Factor changes" $ do
    it "should apply the factor changes" $ do
      let m = 3
          n = 8
          xs = [0..10]
      sTid <- forkIO serve -- Start the server.
      waitForServer 10
      res <- changeTwice xs n m
      killThread sTid -- Kill the server
      results (res) `shouldBe` (map (*n) xs) ++ (map (*m) xs)

    it "should communicate the factor changes" $ do
      let numListeners = 10
          factors = [ i * 2 | i <- [0 .. 10]]
      sTid <- forkIO serve -- Start the server.
      waitForServer 10
      a <- async $ spawnListeners numListeners (length factors)
      --waitForNConnectedClients 10 (numListeners + 1)
      threadDelay (3 * 10^6)
      spawnChanger factors
      ress <- wait a
      killThread sTid -- Kill the server
      (map changes ress) `shouldBe` replicate numListeners factors
