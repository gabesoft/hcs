-- | Client functions
module Client where

import Control.Concurrent.STM
import Control.Lens
import Network (HostName, PortNumber)
import System.IO (Handle)
import Types

mkClient :: ClientName -> HostName -> PortNumber -> Handle -> STM Client
mkClient name host port handle = do
  c <- newTChan
  k <- newTVar Nothing
  n <- newTVar name
  return
    Client
    { _clientName = n
    , _clientHost = host
    , _clientPort = port
    , _clientHandle = handle
    , _clientKicked = k
    , _clientChan = c
    }

mkClientName :: Integer -> String
mkClientName index = "user" ++ show index

sendMessage :: Client -> Message -> STM ()
sendMessage client msg = writeTChan (client ^. clientChan) msg
