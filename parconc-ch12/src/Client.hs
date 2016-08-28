-- | A client for the server.

module Client where

import qualified Config
import           Control.Concurrent (forkFinally, forkIO, threadDelay)
import           Control.Exception  (IOException, catch, throwIO, try)
import           Control.Monad      (liftM)
import           Message
import           Network
import           System.IO

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

talk :: [Message] -> Handle -> IO ()
talk xs h = mapM_ (hPutMessage h) xs >> hPutMessage h End


-- Wait for the server to be ready
waitForServer :: Int -> IO ()
waitForServer n =
  (connectTo Config.host Config.port >>= \h -> hClose h)
  `catch`
  (\ex -> if n < 0
          then throwIO (ex :: IOException)
          else threadDelay (10^6) >> waitForServer (n - 1)
  )


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



