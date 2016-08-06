-- |

module AsyncFunctorSpec where

import           AsyncFunctor
import           Control.Concurrent
import           Control.Exception
import           Data.Typeable
import           Test.Hspec

data Ch10Exception = Ch10Exception String
  deriving (Typeable, Show)

instance Exception Ch10Exception

boom :: IO a
boom = throw (Ch10Exception "boom")

withMessage :: String -> Selector Ch10Exception
withMessage s (Ch10Exception str)  = s == str

spec :: Spec
spec = do
  describe "async" $ do
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

    it "propagates exceptions to the waiting thread" $ do
      (do
          a <- async $ boom
          wait a
        )
      `shouldThrow` (withMessage "boom")

  describe "waitEither" $ do

    it "returns the first action to complete (FATC)" $ do
      mvar <- newEmptyMVar -- We use this MVar to block the right thread
      la <- async $ return "foo"
      ra <- async $ do takeMVar mvar; return 15
      result <- waitEither la ra
      result `shouldBe` (Left "foo")

    it "throws an exception if the FATC threw an exception" $ (do
      mvar <- newEmptyMVar -- We use this MVar to block the right thread
      la <- async $ boom
      ra <- async $ do takeMVar mvar; return 15
      waitEither la ra)
      `shouldThrow` (withMessage "boom")

  describe "waitAny" $ do

    -- What about wait any with the empty list?

    it "returns the first action to complete (FATC)" $ do
      mvar <- newEmptyMVar -- We use this MVar to block the other threads
      a0 <- async $ return "foo"
      a1 <- async $ do takeMVar mvar; return "bar"
      a2 <- async $ do takeMVar mvar; return "baz"
      result <- waitAny [a0, a1, a2]
      result `shouldBe` "foo"

    it "throws an exception if the FATC threw an exception" $ (do
      mvar <- newEmptyMVar
      a0 <- async $ boom
      a1 <- async $ do takeMVar mvar; return "bar"
      a2 <- async $ do takeMVar mvar; return "baz"
      waitAny [a0, a1, a2])
      `shouldThrow` (withMessage "boom")

  describe "fmap" $ do
    it "maps a function to the result of the asynchronous computation" $ do
      a <- async (return 1)
      r <- wait ((+1) <$> a)
      r `shouldBe` 2

    it "obeys the identity law" $ do
      a <- async (return 1)
      r <- wait ((id) <$> a)
      r `shouldBe` 1

      -- TODO: here we should make use of QuickCheck.
      -- I need to learn how to test properties of the monadic code.
      -- fmap id a = a

    it "obeys the composition law" $ do
      let f = (+ 2)
          g = (* 29)
      a <- async (return 1)
      r0 <- wait ((f . g) <$> a)
      r1 <- wait ((f <$>) . (g <$>) $ a)
      r0 `shouldBe` r1
        -- a <- async act
        -- How would we test this?
        -- fmap (f . g) a = (fmap f) . (fmap g) $ a
