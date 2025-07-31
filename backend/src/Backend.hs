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
import qualified Data.Aeson as A

import Data.CaseInsensitive (original)
import Data.Maybe (fromMaybe)

import Schema
import Common.Api
import MyJWT

type NamedConn = (Entity User, WS.Connection)

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
  
  BackendRoute_Login :/ () -> do 
    req <- getRequest
    let headers = listHeaders req
        auth = join $ A.decodeStrict <$> getHeader "Authorization" req 
    case auth of 
      Nothing -> liftIO $ putStrLn "Credentials not perceived."
      Just credentials -> do 
        mEUser <- sqlUserPwdExist credentials
        case mEUser of 
          Nothing -> do 
            liftIO $ putStrLn "User not found or wrong password." 
            modifyResponse $ setResponseCode 401
          Just (Entity e usr) -> do 
            let jwt = createJWT $ usr
            modifyResponse $ setContentType "application/json"
            modifyResponse $ addResponseCookie $ mkJWTCookie jwt
            liftIO $ putStrLn "User Auth success."
            modifyResponse $ setResponseCode 200
            writeLBS $ A.encode usr --TE.encodeUtf8 $ (userName $ user') <> " logged in."

  BackendRoute_Websocket_Query :/ params -> do 
    let keys = fst <$> M.toList params 
    writeBS $ TE.encodeUtf8 $ Prelude.foldl (\b a -> b <> ((<> " ") a)) "" keys

  BackendRoute_Websocket :/ user -> do
    -- is user authenticated or visiting?
    mEUser <- sqlUserExist user
    case mEUser of
      Nothing -> do
        modifyResponse $ setResponseStatus 401 "Unauthorized"
        liftIO $ putStrLn "nothing happenninng user not found..../////////"
        writeBS "401 - Unauthorized"
      Just eUser -> do
        liftIO $ putStrLn "User found... going to open socket..."
        WSSnap.runWebSocketsSnap $ wsHandler conns eUser

  -- ^ runWebSocketsSnap is just a bridge — it hands off the PendingConnection to your wsHandler. 
  -- Everything else is up to you. Broadcast messages to all clients, Count or log active connections,
  -- Assign client IDs or session tokens, Or keep chat history...


-- | @type ServerApp = PendingConnection -> IO ()@ is a fucntion type, hence why `pending`
--   appears down here.
wsHandler :: TVar [NamedConn] -> Entity User -> WS.ServerApp
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
    putStrLn $ "--------------------"
    msgJSON <- WSC.receiveData conn
    let msg = A.decode msgJSON :: Maybe WSMessage
    case msg of 
      Just (NewMessage msg') -> do
        conns <- atomically $ readTVar tvarConns
        forM_ conns $ \(eUser, conn) -> WS.sendTextData conn (A.encode (NewMessage msg'))
        return ()
      otherwise -> putStrLn "TODO case for different type of WSMessage"

wsHandlerUnAuth :: TVar [NamedConn] -> WS.ServerApp
wsHandlerUnAuth conns = undefined

-- | checks if the data in the LoginReq is a valid user in the database.
sqlUserPwdExist :: MonadIO m => Credentials -> m (Maybe (Entity User))
sqlUserPwdExist cs = do
  liftIO $ runSqlite myDB $ do
    mEUser <- selectFirst [UserName ==. username cs, UserPwd ==. password cs] []
    return mEUser

sqlUserExist :: MonadIO m => Text -> m (Maybe (Entity User))
sqlUserExist n = do
  liftIO $ runSqlite myDB $ do
    mEUser <- selectFirst [UserName ==. n] []
    return mEUser
