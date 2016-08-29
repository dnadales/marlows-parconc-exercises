-- | A client for the server.

module Client where

import qualified Config
import           Control.Concurrent (forkFinally, forkIO, threadDelay)
import           Control.Exception  (IOException, bracket, catch, throwIO, try)
import           Control.Monad      (liftM)
import           Message
import           Network
import           System.IO
import           Text.Printf        (printf)

listen :: Handle -> IO [Message]
listen h = do
  hSetBuffering h LineBuffering
  loop []
  where
    loop xs = do
      eMessage <- liftM parse (hGetLine h)
      case eMessage of
        Left e -> do
          putStrLn ("Error: " ++ e)
          loop xs
        Right message -> do
          if message == Bye
            then return xs
            else loop (message:xs)

listens :: Handle -> [IO Message]
listens h = listenAct : listens h
  where listenAct = do
            hSetBuffering h LineBuffering
            eMessage <- liftM parse (hGetLine h)
            case eMessage of
              Left e -> return $ Error (show e)
              Right message -> return message

listenN :: Int -> Handle -> IO [Message]
listenN n h = gListenN n [] (listens h)
  where gListenN 0 xs _ = return xs
        gListenN k xs acts = do
          let listenAct = head acts
          message <- listenAct
          case message of
            Bye -> return xs
            _ -> gListenN (k - 1) (message : xs) (tail acts)

talk :: [Message] -> Handle -> IO ()
talk xs h = mapM_ (hPutMessage h) xs >> hPutMessage h End

-- | Wait for the server to be ready.
waitForServer :: Int -> IO ()
waitForServer n =
  withSocketsDo $ do
  (connectTo Config.host Config.port >>= \h -> do
      putStrLn $ "Sending message on handle " ++ show h
      hPutMessage h End)
  `catch`
  (\ex -> if n < 0
          then throwIO (ex :: IOException)
          else threadDelay (10^6) >> waitForServer (n - 1)
  )


-- | Wait till the server has the given number of connected clients.
waitForNConnectedClients :: Int -> Int -> IO ()
waitForNConnectedClients n numClients = withSocketsDo $ do
  bracket (connectTo Config.host Config.port) (\h -> hPutMessage h End) (loop n)
  where loop k h=  do
          hSetBuffering h LineBuffering
          hPutMessage h GetNClients
          putStrLn $ "Trying to read from " ++ (show h)
          eMessage <- hGetMessageSafe h -- WARNING: we are blocking here!
          case eMessage of
            Right (NClients clientsAtServer) -> do
              printf "clients at server %d, expected clients %d." clientsAtServer numClients
              if (numClients <= clientsAtServer)
                then return ()
                else threadDelay (10^6) >> loop (k - 1) h
            _  -> threadDelay (10^6) >> loop (k - 1) h


interact :: [Message] -> IO [Message]
interact xs = withSocketsDo $ do
  h <- connectTo Config.host Config.port
  _ <- forkIO $ talk xs h
  res <- try (listen h) :: IO (Either IOException [Message])
  -- putStrLn $ "Terminating client:" ++ show res
  hClose h
  case res of
    Left e -> return [Error (show e)]
    Right rs -> return (reverse rs)

-- | Listen a given number of messages.
justListen :: Int -> IO [Message]
justListen n = withSocketsDo $ do
  h <- connectTo Config.host Config.port
  res <- try (listenN n h) :: IO (Either IOException [Message])
  hClose h
  case res of
    Left e -> return [Error (show e)]
    Right rs -> return (reverse rs)



