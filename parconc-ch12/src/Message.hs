-- |

module Message where

import           Text.Read

data Message = End              -- End the session with the server.
             | Bye              -- The session was ended at the server.
             | Factor Integer   -- Change the multiplier factor.
             | Result Integer   -- The value returned by the server.
             | Multiply Integer -- Multiply the number by the multiplier factor.
             | Error String     -- Used to communicate errors.

instance Show Message where
  show End          = "end"
  show Bye          = "bye"
  show (Factor n)   = '*' : (show n)
  show (Multiply n) = show n
  show (Result n)   = '=' : show n
  show (Error e)    = '!' : e

parse :: String -> Either String Message
parse "end" = Right End
parse "bye" = Right Bye
parse ('!' : xs) = Right (Error xs)
parse ('*' : xs) = readEither xs >>= \i -> Right (Factor i)
parse ('=' : xs) = readEither xs >>= \i -> Right (Result i)
parse xs = readEither xs >>= \i -> Right (Multiply i)
