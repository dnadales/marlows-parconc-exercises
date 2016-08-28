-- | Spawn multiple clients.

module Horde where

import           Client
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad
import           Message

-- | 'flood xs n' spawns 'n' clients. It returns all the results of the clients.
flood :: [Message] -> Int -> IO [[Message]]
flood xs n = do
  res <- mapConcurrently Client.interact (replicate n xs)
  return res

changeTwice :: [Integer] -> Integer -> Integer -> IO [Message]
changeTwice xs n m =
  Client.interact commands
  where
    commands = [Factor n] ++ (map Multiply xs) ++ [Factor m] ++ (map Multiply xs)

-- | Spawn listeners, and return their results.
spawnListeners :: Int -> Int -> IO [[Message]]
spawnListeners numListeners numMessages = do
  res <- mapConcurrently Client.justListen (replicate numListeners numMessages)
  return res

-- | Spawn a process that changes the factors.
spawnChanger :: [Integer] -> IO ()
spawnChanger factors = Client.interact commands >> return ()
  where commands = (map Factor factors) ++ [End]
