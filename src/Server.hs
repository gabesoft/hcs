-- | Server functions
module Server where

import Client
import Control.Concurrent.STM
import Control.Lens
import Data.Map.Strict as M
import Network (HostName, PortNumber)
import System.IO (Handle)
import Types

mkServer :: PortNumber -> IO Server
mkServer port = do
  cs <- newTVarIO M.empty
  ct <- newTVarIO 0
  rs <- newTVarIO M.empty
  return Server {_clients = cs, _rooms = rs, _counter = ct, _serverPort = port}

broadcast :: (Server -> STM ClientMap) -> Server -> Message -> STM ()
broadcast f server msg = do
  clientMap <- f server
  mapM_ (flip sendMessage msg) (M.elems clientMap)

allClients :: Server -> STM ClientMap
allClients server = readTVar (server ^. clients)

allClientsExcept :: Client -> Server -> STM ClientMap
allClientsExcept client server = do
  clientMap <- readTVar (server ^. clients)
  name <- readTVar (client ^. clientName)
  return $ M.delete name clientMap

allClientsInRoom :: RoomName -> Server -> STM ClientMap
allClientsInRoom room server = do
  roomsMap <- readTVar (server ^. rooms)
  return $ maybe M.empty id (M.lookup room roomsMap)
