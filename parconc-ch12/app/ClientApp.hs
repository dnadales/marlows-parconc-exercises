-- |

module Main where

import qualified Client

main :: IO ()
main = do
  res <- Client.interact (map show [0..100])
  putStrLn (show res)
