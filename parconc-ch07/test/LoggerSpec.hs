module LoggerSpec where

import           Data.ByteString.Char8
import           Logger
import           System.IO
import           System.Posix.Redirect
import           Test.Hspec

spec :: Spec
spec = do

  describe "Logging service" $ do

    it "should send the log messages to standard output" $ do
      hSetBuffering stdout NoBuffering
      (loggedMessages, _) <- redirectStdout $
        do
          logger <- initLogger
          logMessage logger "hello"
          logMessage logger "logger"
          -- Read from the standard output
          -- the standard output should have the message we logged
          logStop logger
      loggedMessages `shouldBe` pack "hello\nlogger\nlogger: stop\n"


