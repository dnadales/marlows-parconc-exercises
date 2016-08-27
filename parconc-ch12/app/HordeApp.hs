-- |

module Main where

import           Horde

main :: IO ()
main = do
  res <- flood (map show [0..100]) 10
  mapM_ (putStrLn . show) res

