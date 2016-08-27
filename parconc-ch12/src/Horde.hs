-- | Spawn multiple clients.

module Horde where

import           Client
import           Control.Monad

-- | 'flood xs n' spawns 'n' clients. It returns all the results of the clients.
flood :: [String] -> Int -> IO [[String]]
flood xs n = do
  res <- replicateM n (Client.interact xs)
  return res
