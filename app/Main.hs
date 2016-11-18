module Main where

import Client
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception (bracket, finally)
import Control.Lens
import Control.Monad (forever, join, when, void)
import qualified Data.Map.Strict as M
import Network
       (HostName, PortID(..), PortNumber, accept, listenOn, withSocketsDo)
import Server
import System.Environment (getArgs)
import System.IO (Handle, hClose, hGetLine, hPutStrLn)
import Text.Printf
import Types

main :: IO ()
main =
  withSocketsDo $ do
    args <- getArgs
    server <- mkServer (read $ head args)
    sock <- listenOn (PortNumber $ server ^. serverPort)
    printf "Server started on port %s\n" (show $ server ^. serverPort)
    forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally
        (initClient server (handle, host, port))
        (const $ hClose handle)

initClient :: Server -> (Handle, HostName, PortNumber) -> IO ()
initClient server clientData =
  bracket (addClient server clientData) (removeClient server) (runClient server)

runClient :: Server -> Client -> IO ()
runClient server client = do
  name <- atomically $ readTVar (client ^. clientName)
  hPutStrLn handle $ "welcome " ++ name
  hPutStrLn handle $ "type /help to see a list of commands"
  clientMap <- atomically $ readTVar (server ^. clients)
  putStrLn ("Total user count: " ++ (show $ M.size clientMap))
  race handler receiver
  putStrLn $ "Client initialized " ++ name
  return ()
  where
    name = client ^. clientName
    handle = client ^. clientHandle
    receiver =
      forever $ do
        msg <- hGetLine handle
        atomically $ sendMessage client (Command msg)
    handler =
      join $
      atomically $ do
        k <- readTVar (client ^. clientKicked)
        case k of
          Just kicker ->
            return
              (hPutStrLn handle $ "<notice> you have been kicked by " ++ kicker)
          Nothing -> do
            msg <- readTChan (client ^. clientChan)
            return $ handleMessage server client msg >>= flip when handler

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client message =
  case message of
    Notice msg -> output ("<notice> " ++ msg)
    Broadcast name msg -> output ("<" ++ name ++ "> " ++ msg)
    Tell name msg -> output ("<" ++ name ++ "> " ++ msg)
    Command msg ->
      case words msg of
        ["/quit"] -> return False
        ["/nick", newName] -> do
          oldName <- atomically $ readTVar (client ^. clientName)
          atomically (rename client server newName)
          atomically (broadcast others server $ renameNotice oldName newName)
          return True
        ["/join", room] -> undefined
        ["/leave", room] -> undefined
        ["/tell", name] -> undefined
        ["/help"] -> undefined
        ["/kick", name] -> const True <$> atomically (kick client server name)
        ('/':_):_ -> output ("Unrecognized command: " ++ msg)
        _ -> do
          name <- atomically $ readTVar (client ^. clientName)
          atomically (broadcast others server $ Broadcast name msg)
          return True
  where
    renameNotice old new = Notice $ old ++ " is now known as " ++ new
    others = allClientsExcept client
    handle = client ^. clientHandle
    output s = (const True) <$> hPutStrLn handle s

kick :: Client -> Server -> ClientName -> STM ()
kick client server kicked = do
  clientMap <- readTVar (server ^. clients)
  case M.lookup kicked clientMap of
    Nothing -> return ()
    Just klient -> do
      name <- readTVar (client ^. clientName)
      swapTVar (klient ^. clientKicked) (Just name)
      return ()

rename :: Client -> Server -> ClientName -> STM ()
rename client server newName = do
  oldName <- readTVar (client ^. clientName)
  swapTVar (client ^. clientName) newName
  modifyTVar' (server ^. clients) (replace oldName newName)
  modifyTVar' (server ^. rooms) (M.map $ replace oldName newName)
  where
    replace oldKey newKey m =
      case M.lookup oldKey m of
        Just val -> M.insert newKey val (M.delete oldKey m)
        Nothing -> m

removeClient :: Server -> Client -> IO ()
removeClient server client =
  atomically $ do
    name <- readTVar (client ^. clientName)
    modifyTVar' (server ^. clients) $ M.delete name
    modifyTVar' (server ^. rooms) $ M.map (M.delete name)
    broadcast allClients server $ Notice (name ++ " has disconnected")

addClient :: Server -> (Handle, HostName, PortNumber) -> IO Client
addClient server (handle, host, port) =
  atomically $ do
    cnt <- readTVar (server ^. counter)
    clientMap <- readTVar (server ^. clients)
    let index = nextClientId (cnt + 1) clientMap
        name = mkClientName index
    client <- mkClient name host port handle
    modifyTVar' (server ^. clients) (M.insert name client)
    swapTVar (server ^. counter) index
    broadcast
      (allClientsExcept client)
      server
      (Notice $ name ++ " has connected")
    return client

nextClientId :: Integer -> ClientMap -> Integer
nextClientId index clientMap
  | M.member (mkClientName index) clientMap = nextClientId (index + 1) clientMap
  | otherwise = index
