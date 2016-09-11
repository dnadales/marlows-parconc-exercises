{-# LANGUAGE OverloadedStrings #-}
module MyChatServerSpec where

import           ChatClient
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import           Data.ByteString.Lazy.Char8 (ByteString, append)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           MyChatServer
import           Test.Hspec

withServer :: IO () -> IO ()
withServer act =
  bracket (startChatServer >>= \cs -> waitForChatServer >> return cs)
  (stopChatServer)
  (const act)

-- | 'sendCommands n name xs' sends the commands in '[xs ++ Quit]' as 'name' to
-- 'n-1' other clients, and returns the responses from the server.
--
-- Remarks:
--
--   - Commands are sent in the same order as the list.
--
--   - Note that the quit command is always appended to the end of the list.
--
--   - The number of clients includes the current client.
--
sendCommands :: Int -> ClientName -> [ChatCommand] -> IO [ChatResponse]
sendCommands nrClients name commands = do
      mClient <- connect name
      -- Wait till all the clients have connected.
      waitForNClients nrClients mClient
      send mClient (commands ++ [Quit])
      -- Wait till all the clients have disconnected.
      waitForNClients 0 mClient
      xs <- getAllResponses mClient
      disconnect mClient
      return xs

-- | 'quittersIn xs' extracts all the quitters names in 'xs'.
quittersIn :: [ChatResponse] -> Set ClientName
quittersIn = undefined

-- | Convenient alias for 'isSubsetOf' (⊆ has unicode code 2286).
(⊆) :: (Ord a) => Set a -> Set a -> Bool
(⊆) = Set.isSubsetOf

clients :: [ClientName]
clients = ["Johan", "Juan", "Sean", "Maria", "Mary", "Marij"]

n :: Int
n = length clients

clientsSet :: Set ClientName
clientsSet = Set.fromList clients

spec :: Spec
spec = around_ withServer $ do
  describe "Connection with clients" $ do
    it "handles a lot of clients" $ do
      -- Here you can compare the performance of 'asyncWith' and 'forkIO'.
      expectationFailure "TODO: write unit tests"

    it "notifies to all other clients that a new client is connected" $ do
      -- For this we have n-clients connecting, waiting till all are connected,
      -- and then quitting. Each client should receive all the names.
      mss <- mapConcurrently (\c -> sendCommands n c []) clients
      let expectedMessages = Set.fromList $
            [Connected c | c <- clients] ++ [Disconnected c | c <- clients]
          mss' = map (Set.fromList) mss
      mss' `shouldSatisfy` all (\ms -> ms ⊆ expectedMessages)
      -- There must be at least one client that will see all others connecting.
      mss' `shouldSatisfy` any (\ms -> ms == expectedMessages)
      -- Additionally, we could check that all the clients saw the
      -- disconnections.

    it "should not allow duplicated names" $ do
      -- All the clients try to get a name
      expectationFailure "TODO: write unit tests"

    -- Other tests:
    --
    -- - it should not allow changing the name.

  describe "/tell" $ do
    it "should send a message to the specified user" $ do
      let teller = "A"
          listener = "B"
          contents = ["foo", "bar", "baz"]
          commands = [Tell listener m | m <- contents]
          messages = [Message teller listener m | m <- contents]
      _ <- forkIO $ sendCommands 2 teller commands >> return ()
      a1 <- async $ sendCommands 2 listener []
      rs <-  wait a1
      filter (isMessage) rs `shouldBe` messages

  describe "/kick" $ do
    it "should disconnect the given user" $ do
      let kicker = "K"
          victim = "V"
      ak <- async $ sendCommands 2 kicker [Kick victim]
      av <- async $ sendCommands 2 victim []
      -- The quitter gets also disconnected when it quits.
      ks <- wait ak
      ks `shouldBe` [Disconnected victim, Disconnected kicker]
      vs <- wait av
      vs `shouldBe` [Disconnected victim, Disconnected kicker]

    -- Other tests...
    -- it "rejects commands from a disconnected user" $ do
    --   expectationFailure "TODO: write unit tests"

    it "should kick only one of two competing clients" $ do
      -- If two clients send '/kick' simultaneously, only one of the clients
      -- should be kicked.
      --
      -- To test this we could have N clients '/kick'-ing each other, and
      -- ensure that only one client survives! We need to prove that a server
      -- that ignores the requests from a kicked client only leaves one
      -- surviving client in this scenario, which could be done using induction
      -- on the number of clients.
      expectationFailure "TODO: write unit tests"

  describe "/quit" $ do
    it "should disconnect the current client" $ do
      let mName = "Juancho"
      xs <- sendCommands 1 mName []
      xs `shouldBe` [Connected mName, Disconnected mName]

    it "should notify the other clients if a given clients disconnects" $ do
      lrss <- mapConcurrently (\c -> sendCommands n c []) clients
      let qss = map quittersIn lrss
      qss `shouldSatisfy` all (⊆ clientsSet)

  describe "message" $ do
    it "should broadcast messages to all clients" $ do
      let messages = map (C.pack . show) [0 .. 100]
          commands = [Broadcast m | m <- messages]
          expectedMessages c =
            Set.fromList
            [Message c0 c m | c0 <- clients, m <- messages, c0 /= c]
      mss <- mapConcurrently (\c -> sendCommands n c commands) clients
      let mss' = map (Set.fromList . (filter isMessage))  mss
      zip clients mss' `shouldSatisfy` all (\(c, ms) -> ms ⊆ expectedMessages c)


