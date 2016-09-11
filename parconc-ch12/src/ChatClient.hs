-- | A client for the chat server.

module ChatClient where

import           MyChatServer

data ChatClient

-- | Creates a new connection with th chat server.
connect :: ClientName -> IO ChatClient
connect = undefined

-- | Sends a chat command to the server.
send :: ChatClient -> [ChatCommand] -> IO ()
send = undefined

-- | Get all the responses from the server, emptying the inbox.
getAllResponses :: ChatClient -> IO [ChatResponse]
getAllResponses = undefined

-- | 'waitForNClients' waits till there are 'n' clients connected.
waitForNClients :: Int -> ChatClient -> IO ()
waitForNClients = undefined

-- | Disconnect the client from the server.
disconnect :: ChatClient -> IO ()
disconnect = undefined
