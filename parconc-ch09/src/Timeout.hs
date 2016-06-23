{-# LANGUAGE DeriveDataTypeable #-}
-- |
module Timeout where

import           Control.Concurrent
import           Control.Exception
import           Data.Typeable
import           Data.Unique

data Timeout = Timeout Unique deriving (Eq, Typeable)

instance Show Timeout where
  show (Timeout _) = "timeout"

instance Exception Timeout

timeout :: Int -> IO a -> IO (Maybe a)
timeout t m
    | t <  0    = fmap Just m
    | t == 0    = return Nothing
    | otherwise = do
        pid <- myThreadId
        u <- newUnique
        let ex = Timeout u
        handleJust
           (\e -> if e == ex then Just () else Nothing)
           (\_ -> return Nothing)
           (bracket (forkIO $ do threadDelay t
                                 throwTo pid ex)
                    (\tid -> throwTo tid ThreadKilled)
                    (\_ -> fmap Just m))
