-- |

module ServerSpec where

import           Control.Concurrent (forkIO)
import           Horde              (changeTwice)
import           Message            (results)
import           Server             (serve)
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
      _ <- forkIO serve
      res <- changeTwice testList n m
      results (res) `shouldBe` (map (*n) xs)++(map (*m) xs)
