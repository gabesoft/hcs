{-# LANGUAGE TemplateHaskell #-}

-- | Types used by the chat application
module Types where

import Control.Concurrent.STM
import Control.Lens
import Data.Map as M
import Network (HostName, PortNumber)
import System.IO (Handle)

type ClientName = String

type ClientMap = M.Map ClientName Client

type RoomName = String

data Message
  -- Message from the server
  = Notice String
  -- Private message from a client to another client or room
  | Tell ClientName
         String
  -- Public message from a client
  | Broadcast ClientName
              String
  -- Command from user
  | Command String
  deriving (Eq, Show)

data Client = Client
  { _clientName :: ClientName
  , _clientHost :: HostName
  , _clientPort :: PortNumber
  , _clientHandle :: Handle
  , _clientKicked :: TVar (Maybe ClientName)
  , _clientChan :: TChan Message
  } deriving (Eq)

makeLenses ''Client

data Server = Server
  { _clients :: TVar ClientMap
  , _rooms :: TVar (M.Map RoomName ClientMap)
  , _counter :: TVar Integer
  , _serverPort :: PortNumber
  } deriving (Eq)

makeLenses ''Server
