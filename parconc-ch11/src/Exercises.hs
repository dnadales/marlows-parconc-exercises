-- | Extra for this chapter.

module Exercises where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Prelude                  hiding (words)

-- | We experiment with a program that kills itself and leaves an orphan
-- child(ren). This behavior can be changed using `withAsync`.

child ::  IO ()
child = threadDelay (2*10^6) >> putStrLn "Hello from child."

-- | If you run parent, you should see the child printing a message after the
-- parent terminates.
parent :: IO ()
parent = do
  _ <- async child
  putStrLn "Parent: the parent will terminate x.x"
  tid <- myThreadId
  killThread tid

-- | The @tidyParent@ terminates the child if an exeption is raised in its
-- thread.
tidyParent :: IO ()
tidyParent = do
  withAsync (child) $ \_ -> do
    putStrLn "Tidy parent: the parent will terminate x.x"
    tid <- myThreadId
    killThread tid

-- | Note that the behaviours of `parent` `tidyParent` will be different if you
-- runfrom another thread (also from the `ghci` repl). However, if you run them
-- from the main thread, their behavior will be the same. This is explained in
-- page 128: "the program terminates when main returns, even if there are other
-- threads still running."

-- Have a process that waits for the results of a list of processes, and
-- returns their sum. Use `concurrently`


-- What if we returned a list of elements. How would these elements be sorted?

-- | Return the list length after a delay proportional to the list size.
worker :: [a] -> IO (Int)
worker xs = threadDelay (result * 10^5) >> return result
  where result = length xs

words = [ "foo bar"
        , "foo bar baz"
        , "foo"
        , "f"]

conc :: IO a -> IO [a] -> IO [a]
conc iox ioxs = do
  (x, xs) <- concurrently iox ioxs
  return (x:xs)


-- | Count the lengts of the lists, concurrently...
listsLengths :: [[a]] -> IO ([Int])
listsLengths xss = foldr conc (return []) (map worker xss)
-- | Take a minute to examine the effect number of threads that will be spawned
-- by `listsLengths`. This funcion will generate a structure of the form:
--
--     w0 `conc` (w1 `conc` (w2 `conc` (return [])))
--
-- when we want to use the list returned by this function, the concurrent
-- evaluation will start.
--
--     thread0: w0 `conc` (w1 `conc` (w2 `conc` (return [])))
--       thread1: w0
--       thread2: (w1 `conc` (w2 `conc` (return [])))
--         thread3: w1
--         thread4: (w2 `conc` (return []))
--           thread5: w2
--           thread6: return []
--
-- So that's more than the 4 threads I would have expected for evaluating 3
-- workers in parallel.

-- Note that `listsLengths` can be implemented with `mapConcurrently`:
listsLengths2 :: [[a]] -> IO ([Int])
listsLengths2 xss = mapConcurrently  worker xss

-- What if we wanted to wait for a list of processes, to get a list of results,
-- but we wanted to process the results as they come?

-- Let's write a function that allows us to run a sequence of asynchronous
-- tasks and get their results as they arrive.
-- getConcurrently :: (Traversable t) => (a -> IO b) -> t a -> IO (t b)


-- TODO: make sure not to leak any threads! Use `withAsync`.
mkAsyncs :: [IO a] -> IO [Async a]
mkAsyncs ios = mapM async ios

-- | This function does not return the results as they become available.
-- Instead it will sort the results by completion time (in a rather inefficient
-- way).
collect :: [Async a] -> IO [a]
collect [] = return []
collect asyncs = do
  (a, r) <- waitAny asyncs
  rs <- collect (filter (/= a) asyncs)
  return (r:rs)
-- | However, such a function might not be useful in the context of concurrent
-- programming. It is better to adopt the same strategy as with futures (in
-- Scala), when we use a flatMap to describe a computation on a future result.
--
--     http://stackoverflow.com/questions/38813012/collecting-the-async-results-as-they-become-available

listsLengths3 :: [[a]] -> IO ([Int])
listsLengths3 xss = do
  asyncs <- mkAsyncs (map worker xss)
  collect asyncs

