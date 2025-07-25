{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Snap
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WSC
import qualified Network.WebSockets.Snap as WSSnap
import Data.Text 
import Control.Monad 
import Data.Time (getCurrentTime)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM 
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ByteString.UTF8 (toString)
import qualified Data.Map.Strict as M (Map, lookup, toList)
import Database.Persist
import Database.Persist.Sqlite
import qualified Data.Text.Encoding as TE

import Schema

type NamedConn = (Text, WS.Connection)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do 
      conns <- liftIO $ atomically $ newTVar ([] :: [NamedConn])
      runSqlite myDB $ do 
        populateDB
        return ()
      serve $ backendHandlers conns
  , _backend_routeEncoder = fullRouteEncoder
  }

backendHandlers :: TVar [NamedConn] -> R BackendRoute -> Snap ()
backendHandlers conns = \case
  BackendRoute_Missing :/ () -> writeBS "404"
  BackendRoute_Websocket_Query :/ params -> do 
    let keys = fst <$> M.toList params 
    writeBS $ TE.encodeUtf8 $ Prelude.foldl (\b a -> b <> ((<> " ") a)) "" keys
  BackendRoute_Websocket :/ user -> do 
    -- is user authenticated or visiting?
    mEUser <- sqlUserPwdExist user 
    case mEUser of
      Nothing -> do 
        modifyResponse $ setResponseStatus 401 "Unauthorized"
        liftIO $ putStrLn "nothing happenninng user not found..../////////"
        writeBS "401 - Unauthorized"
      Just eUser -> do 
        liftIO $ putStrLn "User found... going to open socket..."
        WSSnap.runWebSocketsSnap $ wsHandler conns user

  -- ^ runWebSocketsSnap is just a bridge — it hands off the PendingConnection to your wsHandler. 
  -- Everything else is up to you. Broadcast messages to all clients, Count or log active connections,
  -- Assign client IDs or session tokens, Or keep chat history...


-- | @type ServerApp = PendingConnection -> IO ()@ is a fucntion type, hence why `pending`
--   appears down here.
wsHandler :: TVar [NamedConn] -> Text -> WS.ServerApp
wsHandler tvarConns params pending = do 

  let path = toString $ WS.requestPath $ WS.pendingRequest pending
  let req = WS.pendingRequest pending
      path = WS.requestPath req
  putStrLn $ "Request path: " <> show path
  conn <- WS.acceptRequest pending
  forever $ do
    putStrLn $ "--------------------"
    msg <- WSC.receiveData conn :: IO Text
    time <- getCurrentTime
    let strTimedMsg = (pack $ show time) <> ": " <> msg
    WS.sendTextData conn ( strTimedMsg :: Text)

-- | checks if the data in the LoginReq is a valid user in the database.
sqlUserPwdExist :: MonadIO m => Text -> m (Maybe (Entity User))
sqlUserPwdExist name = do
  liftIO $ runSqlite myDB $ do
    mEUser <- selectFirst [UserName ==. name] [] -- , UserPwd ==. (password lr)] []
    return mEUser
