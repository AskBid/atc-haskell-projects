{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Snap

import Data.Text 
import Control.Monad 
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Map.Strict as M (Map, lookup, toList)
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.Trans.Resource (runResourceT)

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM 
import Database.Beam
import qualified Database.Beam.Postgres as P
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Data.Conduit
import qualified Database.Beam.Postgres.Conduit as PC
import qualified Data.Conduit.List as CL
import qualified Network.WebSockets.Snap as WSSnap
import Network.WebSockets (Connection)

import Data.CaseInsensitive (original)
import Data.Maybe (fromMaybe)

import Schema 
import Common.Api
import MyJWT
import Migration
import Query
import Websocket (wsHandler, wsHandlerPublic)


connInfo :: P.ConnectInfo
connInfo = P.ConnectInfo
  { P.connectHost = "localhost"	 
  , P.connectPort = 5432 
  , P.connectUser = "atc_user" 	 
  , P.connectPassword = "atcpassword" 
  , P.connectDatabase = "atc_db"
  }

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do 

      wsConns <- liftIO $ atomically $ newTVar ([] :: [NamedConn])
      wsConnsPublic <- liftIO $ atomically $ newTVar ([] :: [Connection])
      pgConn <- P.connect connInfo
      migrateDB pgConn
      -- P.runBeamPostgresDebug putStrLn pgConn $
      --   autoMigrate defaultPostgresMigrationBackend migration
      populateUsers pgConn
      populateMessages pgConn

      serve $ backendHandlers wsConns wsConnsPublic pgConn
  , _backend_routeEncoder = fullRouteEncoder
  }

-- | routes
backendHandlers 
  :: TVar [NamedConn] 
  -> TVar [Connection] 
  -> P.Connection 
  -> R BackendRoute 
  -> Snap ()
backendHandlers conns pubConns pgConn = \case

  BackendRoute_Missing :/ () -> writeBS "404"
  
  BackendRoute_Login :/ () -> do 
    req <- getRequest
    let auth = join $ A.decodeStrict <$> getHeader "Authorization" req
    case auth of
      Nothing -> do
        liftIO $ putStrLn "BE: Credentials not perceived."
        modifyResponse $ setResponseCode 401
      Just credentials -> do 
        users <- liftIO $ conduitQuery pgConn queryUserCredentials credentials
        case users of
          [] -> do 
            liftIO $ putStrLn "BE: User not found or wrong password."
            modifyResponse $ setResponseCode 401
          (u:_) -> do 
            let jwt = createJWT $ u
            modifyResponse $ setContentType "application/json"
            modifyResponse $ addResponseCookie $ mkJWTCookie jwt
            liftIO $ putStrLn "BE: User Auth success."
            modifyResponse $ setResponseCode 200
            writeBS $ BL.toStrict $ A.encode u 

  BackendRoute_Logout :/ () -> do 
    modifyResponse $ setContentType "application/json"
    let expiredJWTCookie = cookieLogout
    modifyResponse $ addResponseCookie $ expiredJWTCookie
    writeBS $ BL.toStrict $ A.encode (User 0 "dummy" "dummy" :: User)

  BackendRoute_Signup :/ () -> do 
    req <- getRequest
    let auth = join $ A.decodeStrict <$> getHeader "Authorization" req
    case auth of

      Nothing -> do
        liftIO $ putStrLn "BE: Credentials not perceived."
        modifyResponse $ setResponseCode 401

      Just credentials -> do
        let usr = username credentials
            pwd = password credentials
        users <- liftIO $ conduitQuery pgConn queryUserByName usr
        case users of

          [] -> do
            PC.runInsert pgConn $ 
              insert (userTable chatDB) $ 
                insertExpressions [User default_ (val_ usr) (val_ pwd)]
            modifyResponse $ setResponseCode 200
            let msg = "User `"<> usr <>"` was succefully registered. You can now Login. \x2705"
            liftIO $ putStrLn $ unpack msg
            writeBS $ BL.toStrict $ A.encode $ BackendResponse {textOnly = msg}

          (u:_) -> do 
            let msg = usr <> " already exist. No signup possible. \x1F6AB"
            liftIO $ putStrLn $ unpack msg     
            modifyResponse $ setResponseStatus 401 "unauthorized"
            writeBS $ BL.toStrict $ A.encode $ BackendResponse {textOnly = msg}

  BackendRoute_Me :/ () -> do
    mUsername <- verifyJWT
    case mUsername of
      Nothing -> do
        modifyResponse $ setResponseStatus 401 "unauthorized"
        liftIO $ putStrLn "BE: Not Authorised. Please login or signup."
      Just username -> do
        users <- liftIO $ conduitQuery pgConn queryUserByName username 
        case users of
          [] -> do 
            modifyResponse $ setResponseStatus 401 "unauthorized"
            liftIO $ putStrLn "BE: User did not exist."
          (u:_) -> do 
            modifyResponse $ setResponseStatus 200 "OK"
            writeBS $ BL.toStrict $ A.encode u

  BackendRoute_Websocket :/ WebscocketRoute_User :/ user -> do 
    users <- liftIO $ conduitQuery pgConn queryUserByName user 
    case users of
      [] -> do
        modifyResponse $ setResponseStatus 401 "Unauthorized"
        liftIO $ putStrLn "BE: nothing happenninng user not found..../////////"
        writeBS "401 - Unauthorized"
      (u:_) -> do
        liftIO $ putStrLn "Route's User found... going to sender's Auth..."
        -- is user authenticated or visiting?
        mUsername <- verifyJWT
        case mUsername of
          Nothing -> do
            modifyResponse $ setResponseStatus 401 "unauthorized"
            liftIO $ putStrLn $ "BE: JWT not perceived. Socket not opening."
            writeBS "invalid JWT"
          Just userAuth -> do
            users <- liftIO $ conduitQuery pgConn queryUserByName userAuth 
            case users of
              [] -> do 
                modifyResponse $ setResponseStatus 401 "unauthorized"
                liftIO $ putStrLn $ "BE: Auth not succesful. Socket not opening."
                writeBS "User did not exist."
              (u:_) -> do 
                liftIO $ putStrLn $ "BE: Auth successful, opening socket for: " <>  (unpack $ _userName u)
                modifyResponse $ setResponseStatus 200 "OK"
                writeBS $ BL.toStrict $ A.encode u
        WSSnap.runWebSocketsSnap $ wsHandler conns pubConns u pgConn

  BackendRoute_Websocket :/ WebscocketRoute_Main :/ () -> do 
    writeBS "Connection with no permission to chat."
    WSSnap.runWebSocketsSnap $ wsHandlerPublic conns pubConns pgConn

  -- ^ runWebSocketsSnap is just a bridge — it hands off the PendingConnection to your wsHandler. 
  -- Everything else is up to you. Broadcast messages to all clients, Count or log active connections,
  -- Assign client IDs or session tokens, Or keep chat history...
