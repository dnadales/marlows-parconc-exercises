-- | A simple server.

module Server where

import           Config
import           Control.Concurrent (forkFinally, forkIO)
import           Control.Monad      (forever, liftM)
import           Message
import           Network
import           System.IO
import           Text.Printf

mkAnswer :: Message -> Message
mkAnswer (Multiply n) = Result (2 * n)
mkAnswer c = Error ("unknown command: '" ++ show c ++ "'")

talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering
  loop
  where
    loop = do
      message <- liftM parse (hGetLine h)
      case message of
        Right End -> hPutMessage h Bye
        Right c -> do
          hPutMessage h (mkAnswer c)
          loop
        Left e -> do
          hPutMessage h (Error ("error while parsing the command: " ++ e))
          loop

serve :: IO ()
-- | From the Network.Socket.Internal library: With older versions of the
-- network library on Windows operating systems, the networking subsystem must
-- be initialised using withSocketsDo before any networking operations can be
-- used.
serve = withSocketsDo $ do
  sock <- listenOn Config.port
  printf "Listening on port %s\n" (show port)
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle) $ \_ -> do
      printf "Closing connection with %s: %s\n" host (show port)
      hClose handle
