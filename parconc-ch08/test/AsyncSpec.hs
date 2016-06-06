-- |

module AsyncSpec where

import           Async
import           Test.Hspec

spec :: Spec
spec = do

  describe "async and wait" $ do

    it "performs actions asynchronously and allows to wait for their results" $ do
      let str0 = "hello "
          str1 = " async world"
      a0 <- async $ return str0
      a1 <- async $ return str1
      r0 <- wait a0
      r1 <- wait a1
      r0 ++ r1 `shouldBe` str0 ++ str1

    it "allows to perform wait multiple times" $ do
      let str = "hello"
      a <- async $ return str
      r0 <- wait a
      r1 <- wait a
      r0 ++ r1 `shouldBe` str ++ str
