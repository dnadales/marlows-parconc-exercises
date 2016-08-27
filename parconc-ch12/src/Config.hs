-- | Common configuration.

module Config where

import           Network

port :: PortID
port = PortNumber (fromIntegral 44444)

host :: String
host = "localhost"

