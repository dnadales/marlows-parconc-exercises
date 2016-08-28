-- |

module Main where

import           Horde
import           Message

main :: IO ()
main = do
  res <- flood (map Multiply [0..100]) 10
  mapM_ (putStrLn . show) res
