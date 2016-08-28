-- | A simple server.

module Server where

import           Config
import           Control.Concurrent (MVar, forkFinally, forkIO, newMVar,
                                     putMVar, readMVar, takeMVar, threadDelay)
import           Control.Monad      (forever, liftM)
import           Message
import           Network
import           System.IO
import           Text.Printf

data State = State {factor :: Integer}

newtype StateVar = StateVar (MVar State)

talk :: Handle -> StateVar -> IO ()
talk h (StateVar sv) = do
  hSetBuffering h LineBuffering
  loop
  where
    loop = do
      message <- liftM parse (hGetLine h)
      case message of
        Right End ->
          hPutMessage h Bye
        _  -> do
          handleMessage message
          loop
    -- Handle messages that do not cause server termination.
    handleMessage message = do
      case message of
        Right (Factor n) -> do
          state <- takeMVar sv
          putMVar sv (state {factor = n})
        Right (Multiply n) -> do
          state <- readMVar sv
          hPutMessage h $ Result (factor state * n)
        Right c -> do
          hPutMessage h $ Error ("unknown command: '" ++ show c ++ "'")
        Left e -> do
          hPutMessage h (Error ("error while parsing the command: " ++ e))

serve :: IO ()
-- | From the 'Network.Socket.Internal' library: With older versions of the
-- network library on Windows operating systems, the networking subsystem must
-- be initialised using withSocketsDo before any networking operations can be
-- used.
serve = withSocketsDo $ do
  sock <- listenOn Config.port
  sv <- newMVar (State {factor = 0})
  printf "Listening on port %s\n" (show port)
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle (StateVar sv)) $ \_ -> do
      printf "Closing connection with %s: %s\n" host (show port)
      hClose handle
