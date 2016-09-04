-- |

module ServerSTM where

import           Config
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad
import           Message
import           Network
import           System.IO
import           Text.Printf

serve :: IO ()
serve = withSocketsDo $ do
  bracket acquireSocket releaseSocket doServe
  where
    acquireSocket = listenOn Config.port
    releaseSocket = \socket -> sClose socket
    doServe = \socket -> do
      printf "Listening on port %s\n" (show port)
      tv <- atomically $ newTVar 0
      forever $ do
        (handle, host, port) <- accept socket
        forkFinally (talk handle tv) (handleResult handle)
    handleResult h res = do
      case res of
        Left e -> do
          putStrLn $ show (e :: SomeException)
        _ -> return ()
      hClose h

talk :: Handle -> TVar Integer -> IO ()
talk h tv = do
  hSetBuffering h LineBuffering
  f <- atomically $ readTVar tv
  _ <- race receiveLoop (listenLoop f)
  return ()
  where
    receiveLoop = do
      message <- hGetMessage h
      case message of
        Right End -> do
          hPutMessage h Bye
        _ -> do
          handleMessage message
          receiveLoop
    handleMessage msg = do
      case msg of
        Right (Factor n) -> do
          atomically $ writeTVar tv n
        Right (Multiply n) -> do
          f <- atomically $ readTVar tv
          hPutMessage h $ Result (f * n)
        Right c -> do
          hPutMessage h $ Error ("unknown command: '" ++ show c ++ "'")
        Left e -> do
          hPutMessage h $ Error ("error while parsing: " ++ e)

    listenLoop currentFactor = do
      newFactor <- atomically $ do
        f <- readTVar tv
        if (currentFactor == f)
          then retry
          else return f
      putStrLn $ "Sending factor " ++ (show newFactor)
      hPutMessage h (Factor newFactor)
      listenLoop newFactor

