{-# LANGUAGE OverloadedStrings #-}

module Websocket where 

import qualified Data.Aeson as A
import Control.Monad 
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WSC
import Control.Concurrent.STM 
import qualified Database.Beam.Postgres as P
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import Control.Exception (finally)
import Data.Text as T

import Database.Schema
import Common.Api
import Database.Query

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
      un = T.unpack $ _userName user

  putStrLn $ "BE: Request path: " <> show path
  wsConn <- WS.acceptRequest pending -- TODO send tvarConns to this wsConn only.

  wsConns <- liftIO $ atomically $ readTVar tvarConns
  
  forM_ wsConns $ \(u, oldConn) -> when (u == user) $ 
    WS.sendClose oldConn ("Closing old connection for " <> _userName user)

  atomically $ modifyTVar' tvarConns $ \conns ->
    (user, wsConn) : Prelude.filter ((/= user) . fst) conns

  _ <- broadcastConnectedUsers tvarConns tvarConnsPub

  -- putStrLn $ "BE: -------------------- Socket cycle start ..." <> un
  let loop = forever $ do
        -- putStrLn $  "BE: " <> T.unpack (_userName user) 
                 -- <> " -------------------- Socket cycle new round ..."
        msgJSON <- WSC.receiveData wsConn
        -- BUG: here sometime the loop gets stuck, and even though frontend sends
        -- a message, here is not received anymore. But if other user sends a message it 
        -- works for all clients.
        -- putStrLn "BE: after receive data"
        let msg = A.decode msgJSON :: Maybe WSMessage

        case msg of 
          Just (NewMessage msg') -> do
            -- putStrLn $ "BE: Public Message received. " <> un
            wsConns' <- atomically $ readTVar tvarConns
            -- putStrLn $ un <> ": BE: CONNECTIONs: " <> (show $ (fst <$> wsConns')) 
            forM_ wsConns' $ \(_, wsConn') -> 
              WS.sendTextData wsConn' (A.encode (NewMessage msg'))
            -- putStrLn $ "BE: Public Message broadcastes. - " <> un
            -- insertFromFEMessage msg' pgConn 
            -- putStrLn $ "BE: Public Message saved on DB. - " <> un
            return ()
          Just (NewPrivate msg') -> do 
            -- putStrLn $ "BE: Private Message received. - " <> un
            broadcastPrivate pgConn tvarConns msg'
          _ -> putStrLn "BE: TODO case for different type of WSMessage"
  finally loop $ do
    atomically $ modifyTVar' tvarConns (Prelude.filter ((/= user) . fst))
    _ <- broadcastConnectedUsers tvarConns tvarConnsPub
    putStrLn $ "BE: " <> T.unpack (_userName user) <> " WebSocket connection closed"

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
  putStrLn $ "BE: CONNECTIONs: " <> (show $ Prelude.length conns)
  let connectedUsers = fst <$> conns
  forM_ conns $ 
    \(user, wsConn) -> do
      liftIO $ putStrLn $ T.unpack "BE: broadcasting users connections to: " 
        <> (T.unpack $ _userName user)
      liftIO $ putStrLn $ T.unpack "BE: sent connected users: " 
        <> show connectedUsers  
      WS.sendTextData wsConn (A.encode (ConnectedClients connectedUsers))
  forM_ connsPublic $ 
    \(_, wsConn) -> 
      WS.sendTextData wsConn (A.encode (ConnectedClients connectedUsers))
  return ()

broadcastPrivate :: P.Connection -> TVar [NamedConn] -> FEMessage -> IO ()
broadcastPrivate pgConn tvarConns fem = do 
  recipients <- queryRecipients (femPrivate fem)
  let viewers = (femOwner fem):recipients
  conns <- atomically $ readTVar tvarConns
  let selectConns = Prelude.filter (\(u, _) -> u `elem` viewers) conns
  forM_ selectConns $ \(_, wsConn) -> 
    WS.sendTextData wsConn (A.encode (NewPrivate fem))
  where
    queryRecipients Nothing      = pure []
    queryRecipients (Just names) = conduitQuery pgConn queryUsersByNames names
