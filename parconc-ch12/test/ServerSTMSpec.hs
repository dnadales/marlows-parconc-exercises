-- |

module ServerSTMSpec where

import           Client
import           Control.Concurrent
import           Control.Concurrent.Async
import           Horde
import           Message
import           ServerSTM
import           Test.Hspec
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

    -- WARNING: this might be a too strong requirement. I think the solution at
    -- Marlow's book could fail to communicate factor changes. However it does
    -- seem to guarantee that the factor changes are reported in the right
    -- order.
    -- it "should communicate the factor changes" $ do
    --   let numListeners = 10
    --       factors = [ i * 2 | i <- [0 .. 10]]
    --   sTid <- forkIO serve -- Start the server.
    --   waitForServer 10
    --   a <- async $ spawnListeners numListeners (length factors)
    --   -- NOTE: to simplify the logic at the server we just introduce a thread
    --   -- delay here to allow the clients to connect before we start sending the
    --   -- factor changes.
    --   threadDelay (3 * 10^6)
    --   spawnChanger factors
    --   ress <- wait a
    --   killThread sTid -- Kill the server
    --   (map changes ress) `shouldBe` replicate numListeners factors
