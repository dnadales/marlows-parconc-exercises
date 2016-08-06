-- | Extra for this chapter.

module Exercises where

import           Control.Concurrent
import           Control.Concurrent.Async

-- | TODO: Make a program that kills itself and leaves an orphan child(ren).
-- Change this behavior using `withAsync`.

child ::  IO ()
child = threadDelay (2*10^6) >> putStrLn "Hello from child."

-- | If you run parent, you should see the child printing a message after the
-- parent terminates.
parent :: IO ()
parent = do
  _ <- async child
  putStrLn "The parent will terminate x.x"
  tid <- myThreadId
  killThread tid

-- | The @tidyParent@ terminates the child if an exeption is raised in its
-- thread.
tidyParent :: IO ()
tidyParent = do
  withAsync (child) $ \_ -> do
    putStrLn "The parent will terminate x.x"
    tid <- myThreadId
    killThread tid

-- Have a process that waits for the results of a list of processes, and
-- returns their sum. Use `concurrently`

-- What if we returned a list of elements. How would these elements be sorted?

-- What if we wanted to wait for a list of processes, to get a list of results,
-- but we wanted to process the results as they come?

