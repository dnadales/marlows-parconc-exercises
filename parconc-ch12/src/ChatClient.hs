-- | A client for the chat server.

module ChatClient where

import           Config
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           MyChatServer
import           Network
import           System.IO


data ClientState =
  ClientState { numClients :: Int -- | Number of clients connected at the chat
                                  -- server.
              }

data ChatClient = ChatClient
  { ccTid     :: ThreadId
  , ccHandle  :: Handle
  , ccChan    :: TChan ChatResponse
  , ccTVState :: TVar ClientState
  }

-- | Creates a new connection with th chat server.
connect :: ClientName -> IO ChatClient
connect name = do
  h <- connectTo Config.host Config.port
  -- TODO: for now we assume that setting the name will always succeed.
  (h ! SetName name)
  tchan <- newTChanIO
  tvar <- newTVarIO (ClientState 0)
  tId <- forkIO $ listen h tchan tvar
  return (ChatClient tId h tchan tvar)

-- | Sends a list of chat commands to the server.
send :: ChatClient -> [ChatCommand] -> IO ()
send cc cs = mapM_ (h ! ) cs
  where h = ccHandle cc

-- | Get all the responses from the server, emptying the inbox.
getAllResponses :: ChatClient -> IO [ChatResponse]
getAllResponses cc = do
  emptyChan (ccChan cc) []
  where emptyChan tchan xs = do
          mValue <- atomically $ tryReadTChan tchan
          case mValue of
            Nothing -> return xs
            Just v -> emptyChan tchan (v:xs)

-- | 'waitForNClients' waits till there are 'n' clients connected.
waitForNClients :: Int -> ChatClient -> IO ()
waitForNClients n cc= do
  let tvar = ccTVState cc
  -- TODO: we could include a timeout.
  atomically $ do
    state <- readTVar tvar
    writeTVar tvar state
    if (numClients state == n)
      then return ()
      else retry



-- | Disconnect the client from the server.
disconnect :: ChatClient -> IO ()
disconnect (ChatClient tId h _ _) = do
  h ! Close
  hClose h
  killThread tId

listen :: Handle -> TChan ChatResponse -> TVar ClientState  -> IO ()
listen h c st = forever $ do
  msg <- receive h
  updateState msg
  -- TODO: if the message is `NrOnlineUsers` update the number of online users accordingly
  -- This requires a shared variable, like a TVar.
  atomically $ writeTChan c msg
  where
    updateState (NrOnlineUsers n) = atomically $
      modifyTVar st (\state -> state {numClients = n})
    updateState _ = return ()

