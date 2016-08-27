-- | A simple server.

module Server where

import           Config
import           Control.Concurrent (forkFinally, forkIO)
import           Control.Monad      (forever)
import           Network
import           System.IO
import           Text.Printf

talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering
  loop
  where
    loop = do
      line <- hGetLine h
      if line == "end"
        then hPutStrLn h "bye"
        else do hPutStrLn h (show $ 2 * (read line :: Integer))
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
