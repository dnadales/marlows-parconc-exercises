-- | Server that communicates back to the clients.x

module ServerIO where

import           Config
import           Control.Concurrent (MVar, forkFinally, forkIO, modifyMVar,
                                     modifyMVar_, newMVar, putMVar, readMVar,
                                     takeMVar, threadDelay)
import           Control.Exception  (IOException, SomeException, bracket,
                                     handle, onException, throw)
import           Control.Monad      (forever, liftM)
import           Data.List          (delete)
import           Message
import           Network
import           System.IO
import           Text.Printf

data State = State {factor :: Integer, handles :: [Handle]}

emptyState :: State
emptyState = State {factor = 0, handles = []}

addHandle :: Handle -> State -> State
addHandle handle state = state {handles = handle : (handles state)}

removeHandle :: Handle -> State -> State
removeHandle handle state = state {handles = delete handle (handles state)}

newtype StateVar = StateVar (MVar State)

talk :: Handle -> StateVar -> IO ()
talk h (StateVar sv) = do
  hSetBuffering h LineBuffering
  loop
  where
    loop = do
      message <- liftM parse (hGetLine h)
      case message of
        Right End -> do
          putStrLn $ "Saying goodbye. Handle " ++ show h
          hPutMessage h Bye
        _  -> do
          handle (\e -> do
                     putStrLn $ "Error when handling " ++ show message
                     putStrLn $ "Error was: " ++ show (e :: SomeException)
                     throw e
                 )
            (handleMessage message)
          loop
    -- Handle messages that do not cause server termination.
    handleMessage message = do
      case message of
        Right (Factor n) -> do
          state <- modifyMVar sv $ \state -> do
                                     let newState = state {factor = n}
                                     return (newState, newState)
          notifyFactorChange n (handles state)
        Right (Multiply n) -> do
          state <- readMVar sv
          hPutMessage h $ Result (factor state * n)
        Right (GetNClients) -> do
          putStrLn "Got 'GetNClients'"
          state <- readMVar sv
          hPutMessage h $ NClients (length (handles state))
        Right c -> do
          hPutMessage h $ Error ("unknown command: '" ++ show c ++ "'")
        Left e -> do
          hPutMessage h (Error ("error while parsing: " ++ e))

notifyFactorChange :: Integer -> [Handle] -> IO ()
notifyFactorChange n hs = mapM_ (forkIO) (map (`hPutMessage` (Factor n)) hs)


serve :: IO ()
serve = withSocketsDo $ do
  bracket acquireSocket releaseSocket doServe
  where
    acquireSocket = listenOn Config.port
    releaseSocket = \socket -> sClose socket
    doServe = \socket -> do
      sv <- newMVar emptyState
      printf "Listening on port %s\n" (show port)
      forever $ do
        (handle, host, port) <- accept socket
        -- Add the handle to the list of handles.
        modifyMVar_ sv (\state -> return $ addHandle handle state)
        printf "Accepted connection from %s: %s - handle %s\n" host (show port) (show handle)
        forkFinally (talk handle (StateVar sv)) $ \_ -> do
          printf "Closing connection with %s: %s\n" host (show port)
          modifyMVar_ sv (\state -> return $ removeHandle handle state)
          hClose handle

