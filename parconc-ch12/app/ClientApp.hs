-- |

module Main where

import qualified Client

main :: IO ()
main = Client.interact (map show [0..100])
