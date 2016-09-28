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
  , receive
  , commandParser
  , Serializable (..)
  ) where

import           Config
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TChan.ReadOnly
import           Control.Exception
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8                 as C
import           Data.Char
import qualified Data.Map                              as Map
import           Data.Monoid
import qualified Data.Scientific                       as Scientific
import           Data.Word
import           Network
import           Prelude                               hiding (takeWhile)
import           System.IO


newtype ChatServer = MkChatServer ThreadId

type Content = C.ByteString
type ClientName = C.ByteString

data ChatCommand = Tell ClientName Content
                 | Kick ClientName
                 | Quit                    -- | Quit the connection with the
                                           -- server, but leave the connection
                                           -- with the server open to receive
                                           -- more messages.
                 | Close                   -- | Close the connection with the
                                           -- server. No more messages will be
                                           -- sent.
                 | OnlineUsers             -- | How many users are connected?
                 | Broadcast Content
                 | SetName ClientName      -- | Set a name for the current
                                           -- client.
                 deriving (Show, Eq)


-- TODO: use a serialization library.
class Serializable a where
  serialize :: a -> C.ByteString
  deserialize :: C.ByteString -> a

instance Serializable ChatCommand where
  -- TODO: sanitize input: trim all line breaks from the command and client
  -- names.
  serialize (Tell n c) =  "/tell " <> n <> " " <> c
  serialize (Kick n) =  "/kick  " <>  n
  serialize Quit = "/quit"
  serialize Close = "/close"
  serialize OnlineUsers = "/#?"
  serialize (Broadcast c) = c
  serialize (SetName c) = "/setname " <> c

  deserialize c =
    case result of
      Right x -> x
      Left _ -> Broadcast c
    where result = parseOnly commandParser c

commandParser :: Parser ChatCommand
commandParser =
  tellParser
  <|> kickParser
  <|> (string "/quit" >> endOfInput >> return Quit)
  <|> (string "/close" >> return Close)
  <|> (string "/#?" >> return OnlineUsers)
  <|> setNameParser

tellParser :: Parser ChatCommand
tellParser = do
  _ <- string "/tell"
  skipSpace
  clientName <- takeWhile1 (isAlphaNum)
  skipSpace
  msg <- takeWhile (const True)
  endOfInput
  return $ Tell clientName msg

kickParser :: Parser ChatCommand
kickParser = do
  _ <- string "/kick"
  skipSpace
  who <- takeWhile1 (isAlphaNum)
  endOfInput
  return $ Kick who

setNameParser :: Parser ChatCommand
setNameParser = do
  _ <- string "/setname"
  skipSpace
  who <- takeWhile1 (isAlphaNum)
  endOfInput
  return $ SetName who

data ChatResponse = Message {from :: ClientName, to :: ClientName, body:: Content}
                  | Connected ClientName
                  | Disconnected ClientName
                  | NrOnlineUsers Int -- | Response with the number of active users.
                  | Error Content     -- | Report an error.
                  deriving (Eq, Show, Ord)

instance Serializable ChatResponse where
  serialize (Message sdr rvr msg) =
    "/message " <> sdr <> " "<> rvr <> " " <> msg
  serialize (Connected who) = "/connected " <> who
  serialize (Disconnected who) = "/disconnected " <> who
  serialize (NrOnlineUsers n) = "/# " <> C.pack (show n)
  serialize (Error c) = "/error " <> c

  deserialize c =
    case result of
      Right x -> x
      -- TODO: we need to consider what happens when we cannot deserialize.
      Left e -> Error $ "unknown command: " <> C.pack e
      where result = parseOnly responseParser c

responseParser :: Parser ChatResponse
responseParser =
  messageParser
  <|> whoParser "/connected" Connected
  <|> whoParser "/disconnected" Disconnected

messageParser :: Parser ChatResponse
messageParser = do
  _ <- string "/message"
  skipSpace
  sdr <- takeWhile1 (isAlphaNum)
  skipSpace
  rvr <- takeWhile1 (isAlphaNum)
  skipSpace
  msg <- takeWhile (const True)
  return $ Message sdr rvr msg

whoParser :: C.ByteString -> (C.ByteString -> ChatResponse) -> Parser ChatResponse
whoParser prefix f = do
  _ <- string prefix
  skipSpace
  who <- takeWhile1 (isAlphaNum)
  endOfInput
  return $ f who

nrUsersParser :: Parser ChatResponse
nrUsersParser = do
  _ <- string "/#"
  skipSpace
  nr <- scientific
  return $ NrOnlineUsers (Scientific.base10Exponent nr)

isMessage :: ChatResponse -> Bool
isMessage (Message _ _ _) = True
isMessage _ = False

-- | Send a chat command to the handle.
(!) :: Handle -> ChatCommand -> IO ()
h ! c = C.hPutStrLn h (serialize c)

-- | Receive a message on the handle.
receive :: (Serializable a) => Handle -> IO a
receive h  = liftM deserialize (C.hGetLine h)

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

data ServerState =
  ServerState { socket        :: Socket
              , numClients    :: TVar Int -- | Number of clients connected at the
                                     -- chat server.
              , clients       :: TVar [Map.Map Handle ClientName]
              , broadcastChan :: TChan ChatResponse
              }

data Client =
  Client { clientHandle :: Handle
          -- | Channel where we receive the messages for the client:
         , clientChan   :: TChan ChatResponse
         }

-- | Associate the client name to the handle in the server state.
addName :: Client -> ClientName -> ServerState -> IO ()
addName = undefined

-- | Return the name associated to the client for that handle.
clientNameForHandle :: Handle -> ServerState -> IO (Maybe ClientName)
clientNameForHandle = undefined

-- | Broadcast a message to all clients. The client name specifies the sender
-- of the message.
broadcastMsg :: ClientName -> Content -> ServerState -> IO ()
broadcastMsg = undefined

talk :: Client -> ServerState -> IO ()
talk c tv = do
  let h = clientHandle c
  cmd <- receive h
  case cmd of
    SetName name -> addName c name tv
    Broadcast msg -> do
      mClientName <- clientNameForHandle h tv
      case mClientName of
        Just clientName ->
          broadcastMsg clientName msg tv
        Nothing -> do
          let hName = C.pack (show h)
              errMsg = Error $ "no client name associated to handle " <> hName
          atomically $ writeTChan (clientChan c) errMsg
    Tell client msg -> undefined
    OnlineUsers -> undefined -- Return the number of online users to the client.
    Kick name -> undefined
    Quit -> undefined
    Close -> undefined
  putStrLn $ (show h) ++ " bla bla bla ..."
  defaultDelay
  if cmd == Close then talk c tv else undefined -- cleanup if the message was close

serve :: IO ()
serve = withSocketsDo $ do
  bracket initializeServer terminateServer doServe
  where
    initializeServer = do
      putStrLn "Starting chat server"
      sock <- listenOn Config.port
      numClientsTVar <- newTVarIO 0
      clientsTVar <- newTVarIO empty
      bChan <- newBroadcastTChanIO
      return ServerState { socket = sock
                         , numClients = numClientsTVar
                         , clients = clientsTVar
                         , broadcastChan = bChan
                         }

    terminateServer state = do
      sClose (socket state)
      putStrLn "Stopping chat server"

    doServe state = do
      (h, host, port) <- accept (socket state)
      duppedChan <- atomically $ dupTChan (broadcastChan state)
      let newClient = Client {clientHandle = h, clientChan = duppedChan}
      withAsync (talk newClient state `finally` hClose h) (\_ -> doServe state)

    -- doServe socket = forever $ do
    --     (h, host, port) <- accept socket
    --     a <- async $ talk h `finally` hClose h
    --     -- How do we cancel all the asynchronous processes if an exception is
    --     -- raised?
    --     return ()

