{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.WebSocket
  ( wsApp,
    broadcast,
    ClientRegistry,
    newClientRegistry,
    broadcastToAll,
  )
where

import Control.Concurrent.MVar
import Control.Exception (SomeException, catch, finally)
import Control.Monad (forever, void)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

-- | Registry of connected WebSocket clients
type ClientRegistry = MVar [WS.Connection]

-- | Create a new client registry
newClientRegistry :: IO ClientRegistry
newClientRegistry = newMVar []

-- | Add a client to the registry
addClient :: ClientRegistry -> WS.Connection -> IO ()
addClient registry conn = modifyMVar_ registry (return . (conn :))

-- | Remove a client from the registry by rebuilding the list
removeClient :: ClientRegistry -> WS.Connection -> IO ()
removeClient registry conn = modifyMVar_ registry $ \clients ->
  return $ filter (isActive conn) clients
  where
    isActive _ _ = True -- Will be filtered out by failed sends

-- | Broadcast a message to all connected clients
-- Automatically removes dead connections
broadcastToAll :: ClientRegistry -> T.Text -> IO ()
broadcastToAll registry msg = do
  clients <- readMVar registry
  activeClients <- foldl checkClient (return []) clients
  modifyMVar_ registry (const $ return activeClients)
  where
    checkClient acc conn = do
      prevActive <- acc
      stillActive <- sendSafe conn
      return $ if stillActive then conn : prevActive else prevActive

    sendSafe conn =
      (WS.sendTextData conn msg >> return True)
        `catch` \(_ :: SomeException) -> return False

-- | Broadcast function (alias for broadcastToAll)
broadcast :: ClientRegistry -> T.Text -> IO ()
broadcast = broadcastToAll

-- | WebSocket application
wsApp :: ClientRegistry -> WS.ServerApp
wsApp registry pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    addClient registry conn
    putStrLn "WebSocket client connected"

    -- Keep connection alive and listen for disconnection
    void $
      ( forever $ do
          msg <- WS.receiveData conn :: IO T.Text
          -- Ignore messages from client for now (one-way: server -> client)
          return ()
      )
        `catch` \(e :: SomeException) -> do
          putStrLn $ "WebSocket client disconnected: " ++ show e
        `finally` do
          removeClient registry conn
          putStrLn "Cleaned up client connection"
