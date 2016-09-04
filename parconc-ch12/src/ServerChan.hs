-- | Implementation of the server using channels to send factor changes to the clients.

module ServerChan where

import           Config
import           Control.Concurrent      (forkFinally, forkIO, killThread,
                                          threadDelay)
import           Control.Concurrent.Chan (Chan, dupChan, newChan, readChan,
                                          writeChan)
import           Control.Exception       (SomeException, bracket, catch)
import           Control.Monad           (forever)
import           Message                 (Message (..), hGetMessage,
                                          hPutMessage)
import           Network                 (accept, listenOn, sClose,
                                          withSocketsDo)
import           System.IO               (BufferMode (LineBuffering), Handle,
                                          hClose, hSetBuffering)
import           Text.Printf             (printf)

-- Extra excercise: support terminating the child threads when the main server
-- thread terminates.

serve :: IO ()
serve = withSocketsDo $ do
  bracket acquireSocket releaseSocket doServe
  where
    acquireSocket = listenOn Config.port
    releaseSocket = \socket -> sClose socket
    doServe = \socket -> do
      printf "Listening on port %s\n" (show port)
      ch <- newChan
      forever $ do
        (handle, host, port) <- accept socket
        chDup <- dupChan ch
        forkFinally (talk handle chDup 0) $
          \res -> do
            case res of
              Left e -> do
                putStrLn $ "Server thread aborted with error: " ++ show (e :: SomeException)
              _ -> return ()
            hClose handle

-- Possible exercise: use the reader monad to pass state around (transformer in
-- this case, since we're using the IO monad as well).

talk :: Handle -> Chan Message -> Integer -> IO ()
talk h ch factor =  do
  hSetBuffering h LineBuffering
  listenerTid <- forkIO $ listenForFactorChanges
  loop factor listenerTid
  where
    loop f tid = do
      message <- hGetMessage h
      case message of
        Right End -> do
          hPutMessage h Bye
          killThread tid
        _ -> do
          newFactor <- handleMessage f message
          loop newFactor tid
    handleMessage f m = do
      case m of
        Right (Factor n) -> do
          writeChan ch (Factor n)
          return n
        Right (Multiply n) -> do
          hPutMessage h $ Result (f * n)
          return f
        Right c -> do
          hPutMessage h $ Error ("unknown command: '" ++ show c ++ "'")
          return f
        Left e -> do
          hPutMessage h $ Error ("error while parsing: " ++ e)
          return f
    listenForFactorChanges = do
      Factor n <- readChan ch
      hPutMessage h (Factor n)
      listenForFactorChanges

