{-# LANGUAGE OverloadedStrings #-}

module Websocket where 

import qualified Data.Aeson as A
import Control.Monad 
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WSC
import Control.Concurrent.STM 
import qualified Database.Beam.Postgres as P
import qualified Data.Text.Encoding as TE
import Data.ByteString.UTF8 (toString)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (finally)
import Network.WebSockets (Connection)
import Data.Text as T

import Schema
import Common.Api
import Query

-- | @type ServerApp = PendingConnection -> IO ()@ is a fucntion type, 
--   hence why `pending` appears down here.
wsHandler :: TVar [NamedConn] 
          -> TVar [AnonConn]
          -> User 
          -> P.Connection 
          -> WS.ServerApp
wsHandler tvarConns tvarConnsPub user pgConn pending = do 
  let req = WS.pendingRequest pending
      path = WS.requestPath req

  putStrLn $ "BE: Request path: " <> show path
  wsConn <- WS.acceptRequest pending
  -- TODO send tvarConns to this wsConn only.
  wsConns <- liftIO $ atomically $ readTVar tvarConns
  let newNamedConn = (user, wsConn)
  let wsConnsPlusThis = case elem user (fst <$> wsConns) of
        True  -> wsConns
        False -> newNamedConn : wsConns
  liftIO $ atomically $ writeTVar tvarConns wsConnsPlusThis
  _ <- broadcastConnectedUsers tvarConns tvarConnsPub

  putStrLn $ "BE: -------------------- Socket cycle start ..."
  let loop = forever $ do
        putStrLn $  "BE: " <> T.unpack (_userName user) 
                 <> " -------------------- Socket cycle new round ..."
        msgJSON <- WSC.receiveData wsConn
        let msg = A.decode msgJSON :: Maybe WSMessage

        case msg of 
          Just (NewMessage msg') -> do
            putStrLn "BE: Message received."
            wsConns' <- atomically $ readTVar tvarConns
            forM_ wsConns' $ 
              \(eUser, wsConn) -> 
                WS.sendTextData wsConn (A.encode (NewMessage msg'))
            putStrLn "BE: Message broadcastes."
            insertFromFEMessage msg' pgConn 
            putStrLn "BE: Message saved on DB."
            return ()
          otherwise -> putStrLn "BE: TODO case for different type of WSMessage"
  finally loop $ do 
    atomically $ modifyTVar' tvarConns (Prelude.filter ((/= user) . fst))
    _ <- broadcastConnectedUsers tvarConns tvarConnsPub
    putStrLn $ "BE: " <> T.unpack (_userName user) <> " WebSocket connection closed"
    WS.sendClose wsConn ("BE: " <> _userName user <> " WebSocket closed")

-- | WebSocket solely used for the login/signup forms to give
--   feedback on user existance and online users (the other websocket)
wsHandlerPublic :: TVar [NamedConn] 
                -> TVar [AnonConn]
                -> P.Connection 
                -> WS.ServerApp
wsHandlerPublic tvarConns tvarConnsPub pgConn pending = do
  wsConn <- WS.acceptRequest pending 
  -- TODO send tvarConns to this wsConn only.
  connsNow <- liftIO $ atomically $ readTVar tvarConnsPub

  let wsConnId = case connsNow of
        [] -> 1
        _  -> 1 + Prelude.maximum (fst <$> connsNow)
      connsNowPlusThis = (wsConnId, wsConn) : connsNow

  liftIO $ atomically $ writeTVar tvarConnsPub connsNowPlusThis
  -- _ <- broadcastConnectedUsers tvarConns tvarConnsPub
  conns <- atomically $ readTVar tvarConns
  let connectedUsers = fst <$> conns
  WS.sendTextData wsConn (A.encode (ConnectedClients connectedUsers))

  let loop = forever $ do
        putStrLn "BE: -------------------- public socket"
        msgJSON <- WS.receiveData wsConn
        let msgUserName = TE.decodeUtf8 msgJSON
        users <- liftIO $ conduitQuery pgConn queryUserByName msgUserName
        case users of
          []    -> WS.sendTextData wsConn (A.encode NoUser)
          (u:_) -> WS.sendTextData wsConn (A.encode (UserExist $ _userName u))
  finally loop $ do 
    atomically $ modifyTVar' tvarConnsPub (Prelude.filter ((/= wsConnId) . fst))
    _ <- broadcastConnectedUsers tvarConns tvarConnsPub
    putStrLn "BE: Public WebSocket connection closed"

-- | sends to all connections (auth and public) all the auth-connections.
--   so all connected users can be visualised in the side bar.
broadcastConnectedUsers :: TVar [NamedConn] 
                        -> TVar [AnonConn]
                        -> IO ()
broadcastConnectedUsers tvarConns tvarConnsPub = do
  putStrLn $ "BE: -------------------- broadcastConnectedUsers"
  conns <- atomically $ readTVar tvarConns
  connsPublic <- atomically $ readTVar tvarConnsPub
  let connectedUsers = fst <$> conns
  forM_ conns $ 
    \(user, wsConn) -> do
      liftIO $ putStrLn $ T.unpack (_userName user) <> " sending users: " <> show connectedUsers
      WS.sendTextData wsConn (A.encode (ConnectedClients connectedUsers))
  forM_ connsPublic $ 
    \(_, wsConn) -> 
      WS.sendTextData wsConn (A.encode (ConnectedClients connectedUsers))
  return ()
