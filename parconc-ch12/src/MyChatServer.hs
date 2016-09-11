{-# LANGUAGE OverloadedStrings #-}
-- | My attempt to write a simple chat server, as decribed in Chapter 12.

module MyChatServer
  ( ChatServer
  , startChatServer
  , waitForChatServer
  , stopChatServer
  , defaultDelay
  , ChatCommand (..)
  , ChatResponse (..)
  , ClientName
  , Content
  , isMessage
  , (!)
  , commandParser
  , Serializable (..)
  ) where

import           Config
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as C
import           Data.Char
import           Data.Word
import           Network
import           Prelude                          hiding (takeWhile)
import           System.IO

newtype ChatServer = MkChatServer ThreadId

type Content = C.ByteString
type ClientName = C.ByteString

data ChatCommand = Tell ClientName Content
                 | Kick ClientName
                 | Quit -- | Quit the connection with the server, but leave the
                        -- connection with the server open to receive more
                        -- messages.
                 | Close -- | Close the connection with the server. No more
                         -- messages will be sent.
                 | OnlineUsers -- | How many users are connected?
                 | Broadcast Content
                 deriving (Show)

class Serializable a where
  serialize :: a -> C.ByteString
  deserialize :: C.ByteString -> a


-- TODO: use a serialization package.

-- TODO: sanitize input: trim all line breaks from the command and client
-- names.
instance Serializable ChatCommand where
  serialize (Tell n c) =  "/tell " `C.append` n `C.append` " " `C.append` c
  serialize (Kick n) =  "/kick " `C.append` n
  serialize Quit = "/quit"
  serialize Close = "/close"
  serialize OnlineUsers = "/#?"
  serialize (Broadcast c) = c

  deserialize c =
    case result of
      Right x -> x
      Left _ -> Broadcast c
    where result = parseOnly commandParser c

commandParser :: Parser ChatCommand
commandParser =
  tellParser
  <|> (string "/quit" >> endOfInput >> return Quit)
  <|> (string "/close" >> return Close)
  <|> (string "/#?" >> return OnlineUsers)

tellParser :: Parser ChatCommand
tellParser = do
  _ <- string "/tell"
  skipSpace
  clientName <- takeWhile1 (isAlphaNum)
  skipSpace
  message <- takeWhile (const True)
  endOfInput
  return $ Tell clientName message

data ChatResponse = Message {from :: ClientName, to :: ClientName, body:: Content}
                  | Connected ClientName
                  | Disconnected ClientName
                  | NrOnlineUsers Int -- | Response with the number of active users.
                  deriving (Eq, Show, Ord)

isMessage :: ChatResponse -> Bool
isMessage (Message _ _ _) = True
isMessage _ = False

-- | Send a chat command to the handle.
(!) :: Handle -> ChatCommand -> IO ()
h ! c = C.hPutStrLn h (serialize c)

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

