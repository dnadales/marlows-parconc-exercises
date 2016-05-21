-- | Implementation of unbounded channels, as described in "MVar as a Building
-- Block: Unbounded Channels".

module Chan where

import           Control.Concurrent hiding (Chan, newChan, readChan, writeChan)


data Chan a

-- | Return a new empty channel.
newChan :: IO (Chan a)
newChan = undefined

-- | Reads from a channel. If the channel is empty the process will block until
-- data is available.
readChan :: Chan a -> IO a
readChan = undefined

-- | Write data to a channel.
writeChan :: Chan a -> a -> IO ()
writeChan = undefined

-- | Duplicate a channel.
-- Implementation of multicast channels.
dupChan :: Chan a -> IO (Chan a)
dupChan = undefined

-- | Put a value at the front of a channel.
unGetChan :: Chan a -> a -> IO ()
unGetChan = undefined
