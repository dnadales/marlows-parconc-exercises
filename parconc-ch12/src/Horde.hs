-- | Spawn multiple clients.

module Horde where

import           Client
import           Control.Monad

-- | 'flood xs n' spawns 'n' clients. It returns all the results of the clients.
flood :: [String] -> Int -> IO [[String]]
flood xs n = do
  res <- replicateM n (Client.interact xs)
  return res


-- | Build a change factor command.
-- NOTE: we are not making the commands type-safe. They should in
-- production code ;)
changeFactor :: Int -> String
changeFactor n = '*' : show n

changeTwice :: [Int] -> Int -> Int -> IO [String]
changeTwice xs n m =
  Client.interact commands
  where
    -- FIXME: we're using string concatenation for composing commands. It would
    -- be better to define a 'Command' (or 'Request') abstract data type
    -- (where, among other things, 'changeFactor' would be defined).
    commands = [changeFactor n] ++ (map show xs) ++ [changeFactor m] ++ (map show xs)

