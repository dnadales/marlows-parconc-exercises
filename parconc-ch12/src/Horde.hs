-- | Spawn multiple clients.

module Horde where

import           Client
import           Control.Monad
import           Message

-- | 'flood xs n' spawns 'n' clients. It returns all the results of the clients.
flood :: [Message] -> Int -> IO [[Message]]
flood xs n = do
  res <- replicateM n (Client.interact xs)
  return res

changeTwice :: [Integer] -> Integer -> Integer -> IO [Message]
changeTwice xs n m =
  Client.interact commands
  where
    commands = [Factor n] ++ (map Multiply xs) ++ [Factor m] ++ (map Multiply xs)

