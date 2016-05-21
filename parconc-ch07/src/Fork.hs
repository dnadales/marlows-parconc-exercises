-- | Simple fork example

module Fork (runExample) where

import           Control.Concurrent
import           Control.Monad
import           System.IO

-- | Shows an example on the use of fork
runExample :: IO ()
runExample = do
  forkIO $ replicateM_ 100 (putChar 'A')
  replicateM_ 100 (putChar 'B')
