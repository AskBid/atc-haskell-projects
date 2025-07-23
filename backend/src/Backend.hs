{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Snap
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WSSnap
import Data.Text 
import Control.Monad 
import Data.Time (getCurrentTime)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM 
import Control.Monad.IO.Class (liftIO)
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
    WSSnap.runWebSocketsSnap $ wsHandler conns "placeholder_name"

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
    time <- getCurrentTime
    let strTime = pack $ show time
    WS.sendTextData conn ( strTime :: Text)
    threadDelay 1000000

  -- let nameParams = M.lookup "name" params
  -- case nameParams of
  --   Nothing -> WS.sendTextData conn ("Name of connection parameter went wrong." :: Text)
  --   Just n -> case n of
  --     Nothing -> WS.sendTextData conn ("Name of connection parameter went wrong." :: Text)
  --     Just n' -> do
  --       atomically $ modifyTVar tvarConns $ \tvarList -> ((n', conn):tvarList)
  --       conns <- liftIO $ atomically $ readTVar tvarConns
  --       let (names, conns') = unzip conns
  --       forM_ conns' $ \conn -> forM_ names (WS.sendTextData conn) 
