{-# LANGUAGE BangPatterns #-}
-- |

module PhonebookSpec where

import           Control.Concurrent
import           Phonebook
import           Prelude            hiding (lookup)
import           Test.Hspec

mkNumber :: Integer -> String
mkNumber = concat . map ( : "-") . show

spec :: Spec
spec = do

  describe "Phonebook" $ do

    it "should be able to find what we inserted" $ do
      pbook <- new
      insert pbook "Jane" "99"
      mNumber <- lookup pbook "Jane"
      mNumber `shouldBe` Just "99"

    -- This test case is useful to compare a lazy version of the `insert`
    -- function against a strict implementation. In the cabal file the maximum
    -- ammount of memory is limited (using the `with-rtsopts` flag) in such a
    -- way that this test case will fail on an lazy implmentation of insert.
    it "should not stack overflow" $ do
      pbook <- new
      let
        insertions =
            [insert pbook ("name" ++ show n) (mkNumber n) | n <- [1..5200]]
      sequence_ insertions
      mNumber <- lookup pbook "name99"
      mNumber `shouldBe` Just (mkNumber 99)
      mNumber <- lookup pbook "unkown"
      mNumber `shouldBe` Nothing
