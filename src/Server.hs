-- | Server functions
module Server where

import Client
import Control.Concurrent.STM
import Control.Lens
import Data.Map as M
import Network (HostName, PortNumber)
import System.IO (Handle)
import Types

mkServer port = do
  cs <- newTVarIO M.empty
  ct <- newTVarIO 0
  rs <- newTVarIO M.empty
  return Server {_clients = cs, _rooms = rs, _counter = ct, _serverPort = port}

allClients :: Server -> STM (M.Map ClientName Client)
allClients server = readTVar (server ^. clients)

allClientsInRoom :: RoomName -> Server -> STM ClientMap
allClientsInRoom room server = do
  roomsMap <- readTVar (server ^. rooms)
  return $ maybe M.empty id (M.lookup room roomsMap)

broadcast :: (Server -> STM ClientMap) -> Server -> Message -> STM ()
broadcast f server msg = do
  clientMap <- f server
  mapM_ (flip sendMessage msg) (M.elems clientMap)
