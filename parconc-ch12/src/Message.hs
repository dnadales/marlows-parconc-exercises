-- |

module Message where

import           Control.Monad (liftM)
import           Data.Maybe    (catMaybes)
import           System.IO     (Handle, hGetLine, hPutStrLn)
import           Text.Read

data Message = End              -- End the session with the server.
             | Bye              -- The session was ended at the server.
             | Factor Integer   -- Change the multiplier factor.
             | Result Integer   -- The value returned by the server.
             | Multiply Integer -- Multiply the number by the multiplier factor.
             | Error String     -- Used to communicate errors.
             deriving (Eq)

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

-- | Read a message from the given handle, and parse it.
hGetMessage :: Handle -> IO (Either String Message)
hGetMessage h = liftM parse (hGetLine h)

-- | Put a message in the handle.
hPutMessage :: Handle -> Message -> IO ()
hPutMessage h m = hPutStrLn h (show m)

getResult :: Message -> Maybe Integer
getResult (Result n) = Just n
getResult _ = Nothing

-- | Get the numeric values out of a list of messages.
results :: [Message] -> [Integer]
results = catMaybes . (map getResult)
