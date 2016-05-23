-- |

module MyExceptionsSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception        hiding (bracket, finally, onException)
import           MyExceptions
import           Test.Hspec

myException :: Selector MyException
myException = const True

boom :: IO a
boom = throw $ MyException

noBoom :: IO ()
noBoom = return ()

spec :: Spec
spec = do

  describe "onException" $ do

    it "Throws the same exception." $ do
      onException boom (return ()) `shouldThrow` myException

    it "Only performs the first action if there are no exceptions." $ do
      val <- onException (return 15) (throw MyException)
      val `shouldBe` 15

  describe "bracket" $ do
    let allocate mvar = do putMVar mvar "allocated"; return mvar
        deallocate mvar = do
          -- This is a very important aspect of MVar semantics! The process
          -- that is putting the variable will block if it is empty!
          -- Therefore we need to take the contents before replacing them!
          --
          _ <- takeMVar mvar
          putMVar mvar "deallocated"
          return mvar

    it "Always perfrom the second action." $ do
      mvar <- newEmptyMVar
      a <- async $ do
        bracket (allocate mvar) deallocate (const boom)
          -- We discard the exception since we are deliberately throwing it for
          -- testing purposes.
          `shouldThrow` myException
      _ <- wait a
      status <- takeMVar mvar
      status `shouldBe` "deallocated"
      a' <- async $ bracket (allocate mvar) deallocate (const noBoom)
      _ <- wait a'
      status' <- takeMVar mvar
      status' `shouldBe` "deallocated"

    it "Fails if the first action fails. The other actions are not executed." $ do
      bracket boom
        (error "deallocation should not be called")
        (error "this action should not be called either")
        `shouldThrow` myException

  describe "finally" $ do

    it "Always perform the second action." $ do
      mvar <- newEmptyMVar
      _ <- finally boom (putMVar mvar "done") `shouldThrow` myException
      msg <- takeMVar mvar
      msg `shouldBe` "done"
      _ <- finally noBoom (putMVar mvar "done again")
      msg' <- takeMVar mvar
      msg' `shouldBe` "done again"

