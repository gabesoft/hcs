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
  return
    Client
    { _clientName = name
    , _clientHost = host
    , _clientPort = port
    , _clientHandle = handle
    , _clientKicked = k
    , _clientChan = c
    }

sendMessage :: Client -> Message -> STM ()
sendMessage client msg = writeTChan (client ^. clientChan) msg
