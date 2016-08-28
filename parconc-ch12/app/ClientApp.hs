-- |

module Main where

import qualified Client
import           Message

main :: IO ()
main = do
  res <- Client.interact (map Multiply [0..100])
  putStrLn (show res)
