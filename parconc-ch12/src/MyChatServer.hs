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
  , serve
  ) where

import           Config
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as C
import           Data.Char
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Scientific                  as Scientific
import           Data.Word
import           Network
import           Prelude                          hiding (takeWhile)
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

data ChatResponse = Message {from :: ClientName, body:: Content}
                  | Connected ClientName
                  | Disconnected ClientName
                  | NrOnlineUsers Int -- | Response with the number of active users.
                  | Error Content     -- | Report an error.
                  | Terminate -- | Message used to terminate the client
                  deriving (Eq, Show, Ord)

instance Serializable ChatResponse where
  serialize (Message sdr msg) =
    "/message " <> sdr <> " " <> msg
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
  msg <- takeWhile (const True)
  return $ Message sdr msg

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
isMessage (Message _ _) = True
isMessage _ = False

-- | Send a chat command to the handle.
(!) :: (Serializable a) => Handle -> a -> IO ()
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
      putStrLn ("error while waiting: " ++ (show e))
      defaultDelay
      waitForChatServer

-- | Stop the chat server.
stopChatServer :: ChatServer -> IO ()
stopChatServer (MkChatServer tId) = killThread tId

data ServerState =
  ServerState { socket        :: Socket
              -- | Socket where the server listens for messages.
              -- , clients       :: TVar [Map.Map Handle ClientName]
              , clientsTChans :: TVar (Map.Map ClientName (TChan ChatResponse))
              , broadcastChan :: TChan ChatResponse
              }

data Client =
  Client { clientHandle :: Handle
          -- | Channel where we receive the messages for the client:
         , clientChan   :: TChan ChatResponse
         , clientName   :: TVar (Maybe ClientName)
         , clientClosed :: TVar Bool
         }

-- | Associate the client name to the handle in the server state.
-- addName :: Client -> ClientName -> ServerState -> IO ()
-- addName = undefined

-- | Return the name associated to the client for that handle.
-- clientNameForHandle :: Handle -> ServerState -> IO (Maybe ClientName)
-- clientNameForHandle = undefined

-- | Broadcast a message to all clients. The client name specifies the sender
-- of the message.
-- broadcastMsg :: ClientName -> Content -> ServerState -> IO ()
-- broadcastMsg cname msgBody sstate =
--   atomically $
--   writeTChan (broadcastChan sstate) (Message {from = cname, body = msgBody})

-- withClientName :: Handle -> ServerState -> (ClientName -> IO ()) -> IO ()
-- withClientName h sstate fact = do
--   mClientName <- clientNameForHandle h sstate
--   case mClientName of
--     Just clientName -> fact mClientName
--     Nothing -> do
--       let hName = C.pack (show h)
--           errMsg = Error $ "no client name associated to handle " <> hName
--       atomically $ writeTChan (clientChan c) errMsg

-- | Process that constantly reads the channel and sends the responses via the
-- handle.
forward :: Client -> IO ()
forward client = do
  let h = clientHandle client
      chan = clientChan client
  resp <- atomically $ readTChan chan
  case resp of
    Terminate -> do
      return ()
    _ -> do
      h ! resp
      forward client

-- | Process waits till the a name as been set, and then switches to the
-- operational mode.
waitForName :: Client -> ServerState -> IO ()
waitForName client sstate = do
  let h = clientHandle client
  cmd <- receive h
  case cmd of
    SetName name -> do
      atomically $ do
        writeTChan (broadcastChan sstate) (Connected name)
        modifyTVar (clientsTChans sstate)
          (\ccmap -> Map.insert name (clientChan client) ccmap)
      atomically $ writeTVar (clientName client) (Just name)
      talk client sstate
    _ -> do
      let hName = C.pack (show h)
          errMsg = Error $ "no client name associated to handle " <> hName
      atomically $ writeTChan (clientChan client) errMsg
      waitForName client sstate

talk :: Client -> ServerState -> IO ()
talk client sstate = do
  let h = clientHandle client
  cmd <- receive h
  mName <- readTVarIO (clientName client)
  let name = fromJust mName -- If this throws an exception we want it, since it
                            -- is an illegal state!
  case cmd of
    SetName otherName -> do
      let errMsg = Error $ "name for this client already set (" <> name <> ")"
      atomically $ writeTChan (clientChan client) errMsg
    Broadcast msg -> do
      atomically $
        writeTChan (broadcastChan sstate) (Message {from = name, body = msg})
    Tell otherClientName msg -> do
      clientChsMap <- readTVarIO (clientsTChans sstate)
      if (otherClientName `Map.member` clientChsMap)
        then do
          let clientCh = clientChsMap Map.! otherClientName
          atomically $
            writeTChan clientCh (Message {from = name, body = msg})
        else do
          let errMsg = Error $ "No client named " <> otherClientName
          atomically $ writeTChan (clientChan client) errMsg
    OnlineUsers -> do
      clientChsMap <- readTVarIO (clientsTChans sstate)
      let answer = NrOnlineUsers (Map.size clientChsMap)
      atomically $ writeTChan (clientChan client) answer
    Kick otherClientName -> do
      clientChsMap <- readTVarIO (clientsTChans sstate)
      if (otherClientName `Map.member` clientChsMap)
        then do
          let clientCh = clientChsMap Map.! otherClientName
          atomically $
            writeTChan clientCh Terminate
        else do
          let errMsg = Error $ "No client named " <> otherClientName
          atomically $ writeTChan (clientChan client) errMsg
    Close ->
      atomically $ writeTVar (clientClosed client) True
    Quit -> atomically $ writeTChan (clientChan client) Terminate
  talk client sstate

waitForClose :: Client -> IO ()
waitForClose client = do
  let h = clientHandle client
  cmd <- receive h
  case cmd of
    Close -> atomically $ writeTVar (clientClosed client) True
    otherwise -> do
      let errMsg = Error $
                   "Command " <> (serialize cmd)
                    <> " sent after client has quitted."
      atomically $ writeTChan (clientChan client) Terminate
      waitForClose client

forwardForever :: Client -> IO ()
forwardForever client = forever $ do
  let h = clientHandle client
      chan = clientChan client
  resp <- atomically $ readTChan chan
  h ! resp
  forwardForever client

initThread :: Client -> ServerState -> IO ()
initThread client sstate = do
  forward client `race` waitForName client sstate
  -- | Notify to all the other clients that this client has terminated.
  -- | TODO: I think we don't need to write back the variable (check in
  -- Marlow's book).
  mName <- readTVarIO (clientName client)
  case mName of
    Nothing -> return () -- No need to notify, since no name was set.
    Just name ->
      atomically $ writeTChan (broadcastChan sstate) (Disconnected name)
  closed <- readTVarIO (clientClosed client)
  if (closed)
    then do forwardForever client `race` waitForClose client
            return ()
    else return ()

serve :: IO ()
serve = withSocketsDo $ do
  bracket initializeServer terminateServer doServe
  where
    initializeServer = do
      putStrLn "Starting chat server"
      sock <- listenOn Config.port
      chansMVar <- newTVarIO Map.empty
      bChan <- newBroadcastTChanIO
      return ServerState { socket = sock
                         , clientsTChans = chansMVar
                         , broadcastChan = bChan
                         }

    terminateServer state = do
      sClose (socket state)
      putStrLn "Stopping chat server"

    doServe state = do
      (h, host, port) <- accept (socket state)
      duppedChan <- atomically $ dupTChan (broadcastChan state)
      nameTV <- newTVarIO Nothing
      closedTV <- newTVarIO False
      let newClient = Client { clientHandle = h
                             , clientChan = duppedChan
                             , clientName = nameTV
                             , clientClosed = closedTV }
      withAsync
        (initThread newClient state `finally` hClose h)
        (\_ -> doServe state)

    -- doServe socket = forever $ do
    --     (h, host, port) <- accept socket
    --     a <- async $ talk h `finally` hClose h
    --     -- How do we cancel all the asynchronous processes if an exception is
    --     -- raised?
    --     return ()

