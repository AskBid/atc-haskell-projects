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

import Schema
import Common.Api
import Query

-- | @type ServerApp = PendingConnection -> IO ()@ is a fucntion type, hence why `pending`
--   appears down here.
wsHandler :: TVar [NamedConn] 
          -> TVar [Connection]
          -> User 
          -> P.Connection 
          -> WS.ServerApp
wsHandler tvarConns tvarConnsPub eUser pgConn pending = do 
  let path = toString $ WS.requestPath $ WS.pendingRequest pending
  let req = WS.pendingRequest pending
      path = WS.requestPath req

  putStrLn $ "Request path: " <> show path
  wsConn <- WS.acceptRequest pending

  wsConns <- liftIO $ atomically $ readTVar tvarConns
  let wsConnsPlusThis = (eUser, wsConn) : wsConns
  liftIO $ atomically $ writeTVar tvarConns wsConnsPlusThis

  putStrLn $ "-------------------- broadcastConnectedUsers"
  _ <- broadcastConnectedUsers tvarConns tvarConnsPub

  putStrLn $ "-------------------- Socket cycle start ..."
  forever $ do
    putStrLn $ "-------------------- Socket cycle new round ..."
    msgJSON <- WSC.receiveData wsConn
    let msg = A.decode msgJSON :: Maybe WSMessage

    case msg of 

      Just (NewMessage msg') -> do
        putStrLn "Message received."
        wsConns' <- atomically $ readTVar tvarConns
        forM_ wsConns' $ 
          \(eUser, wsConn) -> 
            WS.sendTextData wsConn (A.encode (NewMessage msg'))
        putStrLn "Message broadcastes."
        insertFromFEMessage msg' pgConn 
        putStrLn "Message saved on DB."
        return ()

      otherwise -> putStrLn "TODO case for different type of WSMessage"

wsHandlerPublic :: TVar [NamedConn] 
                -> TVar [Connection]
                -> P.Connection 
                -> WS.ServerApp
wsHandlerPublic conns pubConns pgConn pending = do
  -- putStrLn "inside public ws handler..."
  conn <- WS.acceptRequest pending 
  let loop = forever $ do
      putStrLn "-------------------- public socket"
      msgJSON <- WS.receiveData conn
      let msgUserName = TE.decodeUtf8 msgJSON
      users <- liftIO $ conduitQuery pgConn queryUserByName msgUserName
      case users of
        [] -> WS.sendTextData conn (A.encode NoUser)
        (u:_) -> WS.sendTextData conn (A.encode (UserExist $ _userName u))
  
  loop `finally` putStrLn "Public WebSocket connection closed"

broadcastConnectedUsers :: TVar [NamedConn] 
                        -> TVar [Connection]
                        -> IO ()
broadcastConnectedUsers tvarConns tvarConnsPub = do
  conns <- atomically $ readTVar tvarConns
  connsPublic <- atomically $ readTVar tvarConnsPub
  let connectedUsers = fst <$> conns
  forM_ conns $ 
    \(eUser, wsConn) -> 
      WS.sendTextData wsConn (A.encode (ConnectedClients connectedUsers))
  forM_ connsPublic $ 
    \wsConn -> 
      WS.sendTextData wsConn (A.encode (ConnectedClients connectedUsers))
  return ()
