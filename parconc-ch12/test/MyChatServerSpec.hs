-- |

module MyChatServerSpec where

import           Control.Exception
import           MyChatServer
import           Test.Hspec

withServer :: IO () -> IO ()
withServer act =
  bracket (startChatServer >>= \cs -> waitForChatServer >> return cs)
  (stopChatServer)
  (const act)

spec :: Spec
spec = around_ withServer $ do
  describe "Connection with clients" $ do
    it "notifies to all other clients that a new client is connected" $ do
      -- For this have n-clients connecting. Each client should receive the
      -- remaining (n-1) names.
      expectationFailure "TODO: write unit tests"

    it "should not allow duplicated names" $ do
      expectationFailure "TODO: write unit tests"

  describe "/tell" $ do
    it "should send a message to the specified user" $ do
      expectationFailure "TODO: write unit tests"

  describe "/kick" $ do
    it "should disconnect the given user" $ do
      expectationFailure "TODO: write unit tests"

    it "rejects commands from a disconnected user" $ do
      expectationFailure "TODO: write unit tests"

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
      expectationFailure "TODO: write unit tests"

    it "should notify the other clients if a given clients disconnects" $ do
      -- Including the client itself.
      expectationFailure "TODO: write unit tests"

  describe "message" $ do
    it "should broadcast messages to all clients" $ do
      -- Ensure that no messages are lost.
      expectationFailure "TODO: write unit tests"

