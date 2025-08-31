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

import Schema
import Common.Api
import Query

-- | @type ServerApp = PendingConnection -> IO ()@ is a fucntion type, hence why `pending`
--   appears down here.
wsHandler :: TVar [NamedConn] -> User -> WS.ServerApp
wsHandler tvarConns eUser pending = do 
  let path = toString $ WS.requestPath $ WS.pendingRequest pending
  let req = WS.pendingRequest pending
      path = WS.requestPath req

  putStrLn $ "Request path: " <> show path
  conn <- WS.acceptRequest pending

  conns <- liftIO $ atomically $ readTVar tvarConns
  let connsPlusThis = (eUser, conn) : conns
  liftIO $ atomically $ writeTVar tvarConns connsPlusThis

  forever $ do
    putStrLn $ "-------------------- Socket cycle start ..."
    msgJSON <- WSC.receiveData conn
    putStrLn "WSMessage received."
    let msg = A.decode msgJSON :: Maybe WSMessage
    case msg of 
      Just (NewMessage msg') -> do
        conns <- atomically $ readTVar tvarConns
        forM_ conns $ \(eUser, conn) -> WS.sendTextData conn (A.encode (NewMessage msg'))
        return ()
      otherwise -> putStrLn "TODO case for different type of WSMessage"

wsHandlerPublic :: TVar [NamedConn] -> P.Connection -> WS.ServerApp
wsHandlerPublic conns pgConn pending = do
  -- putStrLn "inside public ws handler..."
  conn <- WS.acceptRequest pending
  forever $ do 
    putStrLn "--------------------"
    msgJSON <- WSC.receiveData conn
    let msgUserName = TE.decodeUtf8 msgJSON
    users <- liftIO $ conduitQuery pgConn queryUserByName msgUserName
    case users of 
      [] -> WS.sendTextData conn (A.encode NoUser)
      (u:_) -> WS.sendTextData conn (A.encode (UserExist $ _userName u))
    return ()
