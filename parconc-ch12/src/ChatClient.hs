-- | A client for the chat server.

module ChatClient where

import           Config
import           Control.Concurrent.Async
import           MyChatServer
import           Network
import           System.IO

data ChatClient = ChatClient (Async [ChatResponse]) Handle

-- | Creates a new connection with th chat server.
connect :: ClientName -> IO ChatClient
connect name = do
  h <- connectTo Config.host Config.port
  a <- async $ listen h
  return (ChatClient a h)

-- | Sends a list of chat commands to the server.
send :: ChatClient -> [ChatCommand] -> IO ()
send (ChatClient _ h) cs = mapM_ (h ! ) cs

-- | Get all the responses from the server, emptying the inbox.
getAllResponses :: ChatClient -> IO [ChatResponse]
getAllResponses = undefined

-- | 'waitForNClients' waits till there are 'n' clients connected.
waitForNClients :: Int -> ChatClient -> IO ()
waitForNClients = undefined

-- | Disconnect the client from the server.
disconnect :: ChatClient -> IO ()
disconnect = undefined


listen :: Handle -> IO [ChatResponse]
listen = undefined
