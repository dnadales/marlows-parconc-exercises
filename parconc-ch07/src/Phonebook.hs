{-# LANGUAGE BangPatterns #-}
-- | See MVar as a container for shared state.

module Phonebook where

import           Control.Concurrent
import           Data.Map           (Map)

type Name = String
type PhoneNumber = String
type PhoneBook = Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

-- | Make a new phonebook state.
new :: IO PhoneBookState
new = undefined

-- | Insert a new entry in the phone book.
insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
-- Again, since we are altering a shared variable, this function has side
-- effects, and therefore it returns IO ()
insert = undefined

-- | Lookup an entry in the  phonebook.
lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup = undefined
