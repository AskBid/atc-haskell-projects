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
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Database.Beam
import qualified Database.Beam.Postgres as P
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Data.Conduit
import qualified Database.Beam.Postgres.Conduit as PC
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource (runResourceT)

import Data.CaseInsensitive (original)
import Data.Maybe (fromMaybe)

import Schema 
import Common.Api
import MyJWT
import Migration

type NamedConn = (User, WS.Connection)

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
      pgConn <- P.connect connInfo
      migrateDB pgConn
      -- P.runBeamPostgresDebug putStrLn pgConn $
      --   autoMigrate defaultPostgresMigrationBackend migration
      populateUsers pgConn
      populateMessages pgConn

      serve $ backendHandlers wsConns pgConn
  , _backend_routeEncoder = fullRouteEncoder
  }

backendHandlers :: TVar [NamedConn] -> P.Connection -> R BackendRoute -> Snap ()
backendHandlers conns pgConn = \case
  BackendRoute_Missing :/ () -> writeBS "404"
  
  BackendRoute_Login :/ () -> do 
    req <- getRequest
    let auth = join $ A.decodeStrict <$> getHeader "Authorization" req
    case auth of
      Nothing -> do
        liftIO $ putStrLn "Credentials not perceived."
        modifyResponse $ setResponseCode 401
      Just credentials -> do 
        users <- liftIO $ conduitQuery pgConn queryUserCredentials credentials
        case users of
          [] -> do 
            liftIO $ putStrLn "User not found or wrong password."
            modifyResponse $ setResponseCode 401
          (u:_) -> do 
            let jwt = createJWT $ u
            modifyResponse $ setContentType "application/json"
            modifyResponse $ addResponseCookie $ mkJWTCookie jwt
            liftIO $ putStrLn "User Auth success."
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
        liftIO $ putStrLn "Credentials not perceived."
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
            let msg = pack "User was succefully registered. You can now Login."
            liftIO $ putStrLn $ unpack msg
            writeBS $ BL.toStrict $ A.encode $ BackendResponse {textOnly = msg}

          (u:_) -> do 
            liftIO $ putStrLn $ unpack $ username credentials <> " already exist. No signup possible."
            modifyResponse $ setResponseStatus 401 "unauthorized"
            writeBS "User already exist."

  BackendRoute_Me :/ () -> do
    mUsername <- verifyJWT
    case mUsername of
      Nothing -> do
        modifyResponse $ setResponseStatus 401 "unauthorized"
        writeBS "invalid JWT"
      Just username -> do
        modifyResponse $ setResponseStatus 200 "OK"
        users <- liftIO $ conduitQuery pgConn queryUserByName username 
        case users of
          [] -> do 
            modifyResponse $ setResponseStatus 401 "unauthorized"
            writeBS "User did not exist."
          (u:_) -> writeBS $ BL.toStrict $ A.encode u

  BackendRoute_Websocket :/ WebscocketRoute_User :/ user -> do
    -- is user authenticated or visiting?
    users <- liftIO $ conduitQuery pgConn queryUserByName user 
    case users of
      [] -> do
        modifyResponse $ setResponseStatus 401 "Unauthorized"
        liftIO $ putStrLn "nothing happenninng user not found..../////////"
        writeBS "401 - Unauthorized"
      (u:_) -> do
        liftIO $ putStrLn "User found... going to open socket..."
        WSSnap.runWebSocketsSnap $ wsHandler conns u

  BackendRoute_Websocket :/ WebscocketRoute_Main :/ () -> do 
    writeBS "Connection with no permission to chat."
    WSSnap.runWebSocketsSnap $ wsHandlerPublic conns pgConn

  -- ^ runWebSocketsSnap is just a bridge — it hands off the PendingConnection to your wsHandler. 
  -- Everything else is up to you. Broadcast messages to all clients, Count or log active connections,
  -- Assign client IDs or session tokens, Or keep chat history...


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
    putStrLn $ "--------------------"
    msgJSON <- WSC.receiveData conn
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


-- | Run a Beam query and collect all results into a list.
conduitQuery 
  :: P.Connection 
  -> (a -> Q P.Postgres ChatDB QBaseScope (UserT (QExpr P.Postgres QBaseScope))) 
  -> a 
  -> IO [User]
conduitQuery conn query name =
  runResourceT $
    runConduit $
      PC.streamingRunSelect conn (select (query name))
        -- ^ ConduitT () a m ()
        -- Think of it like: “Here’s a stream of rows (Users), you can consume them however you like.”
        .| CL.consume   
        -- ^ collect all rows into a list.

queryUserByName :: Text -> Q P.Postgres ChatDB s (UserT (QExpr P.Postgres s))
-- ^ s is the query scope phantom type. 
--   to track query scoping at the type level, 
--   so you don’t accidentally mix rows from different queries or cross scope 
--   boundaries incorrectly.
--   Think of s like a unique query id at the type level.
--   Every time you start a new query (Q … s …), GHC invents a new s.
--   That way Beam can enforce rules like:
--     You can join rows from the same scope (s matches).
--     You cannot directly compare an expression from query A with query B (s ≠ s').
--   when you “join” two tables in a query, you’re really creating a new derived scope
--   that contains columns from both tables. Conceptually, it’s like a new intermediate table
queryUserByName name = do
  u <- all_ (userTable chatDB)
  guard_ (_userName u ==. val_ name)
  pure u

queryUserCredentials :: Credentials -> Q P.Postgres ChatDB s (UserT (QExpr P.Postgres s))
queryUserCredentials (Credentials usr pwd) = do
  u <- all_ (userTable chatDB)
  guard_ (_userName u ==. val_ usr)
  guard_ (_userPwd u ==. val_ pwd)
  pure u
