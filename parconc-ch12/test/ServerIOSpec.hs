-- |

module ServerIOSpec where

import           Client                   (waitForNConnectedClients,
                                           waitForServer)
import           Control.Concurrent       (forkIO, killThread)
import           Control.Concurrent.Async (async, wait)
import           Horde                    (changeTwice, spawnChanger,
                                           spawnListeners)
import           Message                  (changes, results)
import           ServerIO                 (serve)
import           Test.Hspec

testList :: [Integer]
testList = [0..10]

spec :: Spec
spec = do
  describe "Factor changes" $ do
    it "should apply the factor changes" $ do
      let m = 3
          n = 8
          xs = testList
      sTid <- forkIO serve -- Start the server.
      waitForServer 10
      res <- changeTwice testList n m
      killThread sTid -- Kill the server
      results (res) `shouldBe` (map (*n) xs) ++ (map (*m) xs)

    it "should communicate the factor changes" $ do
      let numListeners = 10
          factors = [ i * 2 | i <- [0 .. 10]]
      sTid <- forkIO serve -- Start the server.
      waitForServer 10
      a <- async $ spawnListeners numListeners (length factors)
      waitForNConnectedClients 10 (numListeners + 1)
      spawnChanger factors
      ress <- wait a
      killThread sTid -- Kill the server
      (map changes ress) `shouldBe` replicate numListeners factors




