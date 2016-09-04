-- | Server implementation as seen in 'server2.hs' in Marlow's book, modified
-- to our setting.

module ServerSTMByMarlow where

import           Config
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Message
import           Network
import           System.IO
import           Text.Printf

-- <<main
serve = withSocketsDo $ do
  bracket acquireSocket releaseSocket doServe
  where
    acquireSocket = listenOn Config.port
    releaseSocket socket = sClose socket
    doServe socket =  do
      printf "Listening on port %s\n" (show Config.port)
      factor <- atomically $ newTVar (-1)                               -- <1>
      forever $ do
        (handle, host, p) <- accept socket
        printf "Accepted connection from %s: %s\n" host (show p)
        forkFinally (talk handle factor) (handleResult handle p)

    handleResult h p res= do
      case res of
        Left e -> do
          putStrLn $ "Server thread aborted with error: " ++ show (e :: SomeException)
            ++ " - host: " ++ host ++ ", port: " ++ show p
        _ ->
          printf "Terminating connection with %s: %s\n" host (show p)
      hClose h


-- >>

-- <<talk
talk :: Handle -> TVar Integer -> IO ()
talk h factor = do
  hSetBuffering h LineBuffering
  c <- atomically newTChan              -- <1>
  _ <- race (server h factor c) (receive h c)  -- <2>
  return ()
-- >>

-- <<receive
receive :: Handle -> TChan Message -> IO ()
receive h c = forever $ do
  msg <- hGetMessage h
         `catch` (\e -> do
                     putStrLn $ "Error at receive "  ++ show (e :: SomeException)
                     throw e
                 )
  case msg of
     Left e -> do
          hPutMessage h (Error ("error while parsing: " ++ e))
     Right command ->
       atomically $ writeTChan c command
-- >>

-- <<server
server :: Handle -> TVar Integer -> TChan Message -> IO ()
server h factor c = do
  f <- atomically $ readTVar factor     -- <1>
  loop f                                -- <3>
 where
  loop f = do
    action <- atomically $ do           -- <4>
      f' <- readTVar factor             -- <5>
      if (f /= f')                      -- <6>
         then return (newfactor f')     -- <7>
         else do
           l <- readTChan c             -- <8>
           return (command f l)         -- <9>
    action

  newfactor f = do                      -- <10>
    -- putStrLn $ "Sending factor " ++ (show f)
    hPutMessage h (Factor f)
    loop f

  command f s                           -- <11>
   = case s of
      End -> do
        putStrLn "Saying goodbye..."
        hPutMessage h Bye
      Factor n -> do
        atomically $ writeTVar factor n -- <13>
        loop f
      Multiply n  -> do
        hPutMessage h $ Result (f * n)
        loop f
      _ -> do
        hPutMessage h $ Error ("unknown command: '" ++ show s ++ "'")
        loop f
-- >>
