-- | A client for the serve.

module Client where

import qualified Config
import           Control.Concurrent (forkFinally, forkIO)
import           Control.Exception  (IOException, try)
import           Network
import           System.IO

listen :: Handle -> IO [String]
listen h = do
  hSetBuffering h LineBuffering
  loop []
  where
    loop xs = do
      line <- hGetLine h
      -- FIXME: here we are opening the doors for inconsistencies. If the
      -- server response that signals the termination of the connection changes
      -- at the server, but we forget to modify it here, this function will not
      -- work as expected.
      if line == "bye"
        then return xs
        else loop (line:xs)

talk :: [String] -> Handle -> IO ()
talk xs h = mapM_ (hPutStrLn h) xs >> hPutStrLn h "end"


interact :: [String] -> IO [String]
interact xs = withSocketsDo $ do
  h <- connectTo Config.host Config.port
  _ <- forkIO $ talk xs h
  res <- try (listen h) :: IO (Either IOException [String])
  putStrLn "Terminating client"
  hClose h
  case res of
    Left e -> return [show e]
    Right rs -> return rs




