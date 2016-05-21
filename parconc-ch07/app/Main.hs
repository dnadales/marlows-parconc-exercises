module Main where

import qualified Fork
import qualified Reminder
import           System.IO

main :: IO ()
main = do
  -- put the handle into non-buffered mode.
  hSetBuffering stdout NoBuffering
  -- Comment/uncomment the examples you want to run:
  -- Fork.runExample
  Reminder.runExample
