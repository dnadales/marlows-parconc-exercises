-- | My attempt to write a simple chat server, as decribed in Chapter 12.

module MyChatServer
  ( ChatServer
  , startChatServer
  , waitForChatServer
  , stopChatServer
  , defaultDelay
  ) where

import           Config
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Network
import           System.IO

newtype ChatServer = MkChatServer ThreadId

defaultDelay :: IO ()
defaultDelay = threadDelay (10^6)

-- | Start the chat server.
startChatServer :: IO ChatServer
startChatServer = liftM MkChatServer $ forkIO serve

-- | Wait for the chat server.
waitForChatServer :: IO ()
waitForChatServer =
  (connectTo Config.host Config.port >>= \h -> hClose h)
  `catch`
  handleIO
  where
    -- handleIO :: IOException -> IO ()
    -- handleIO _ = defaultDelay >> waitForChatServer
    handleIO :: SomeException -> IO ()
    handleIO e = do
      -- putStrLn ("error while waiting: " ++ (show e))
      defaultDelay
      waitForChatServer

-- | Stop the chat server.
stopChatServer :: ChatServer -> IO ()
stopChatServer (MkChatServer tId) = killThread tId

talk :: Handle -> IO ()
talk h = do
  putStrLn $ (show h) ++ " bla bla bla ..."
  defaultDelay
  talk h
-- talk h = return ()

serve :: IO ()
serve = withSocketsDo $ do
  bracket acquireSocket releaseSocket doServe
  where
    acquireSocket = do
      putStrLn "Starting chat server"
      listenOn Config.port

    releaseSocket socket = do
      sClose socket
      putStrLn "Stopping chat server"

    doServe socket = do
      (h, host, port) <- accept socket
      withAsync (talk h `finally` hClose h) (\_ -> doServe socket)

    -- doServe socket = forever $ do
    --     (h, host, port) <- accept socket
    --     a <- async $ talk h `finally` hClose h
    --     -- How do we cancel all the asynchronous processes if an exception is
    --     -- raised?
    --     return ()

