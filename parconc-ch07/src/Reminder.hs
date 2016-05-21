-- | Timed reminders. Extracted from sub-section "A Simple Example: Reminders".

module Reminder where

import           Control.Concurrent
import           Control.Monad
import           Data.Generics.Aliases
import           Data.Maybe
import           System.IO
import           Text.Printf
import           Text.Read             (readMaybe)

runExample :: IO ()
runExample =
  forever $ do
  s <- getLine
  forkIO $ setReminder s

setReminder :: String -> IO ()
setReminder = undefined

