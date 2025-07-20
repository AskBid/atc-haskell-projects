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

type NamedConn = (Text, WS.Connection)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do 
      conns <- liftIO $ atomically $ newTVar ([] :: [NamedConn])
      serve $ backendHandlers conns
  , _backend_routeEncoder = fullRouteEncoder
  }

backendHandlers :: TVar [NamedConn] -> R BackendRoute -> Snap ()
backendHandlers conns r = do
  liftIO $ putStrLn $ "Received backend route: " <> show r
  case r of
    BackendRoute_Missing :/ () -> writeBS "404"
    BackendRoute_Websocket :/ params -> do 
      liftIO $ putStrLn "prooooovaaaaaaaaa"
      WSSnap.runWebSocketsSnap $ wsHandler conns
  -- ^ runWebSocketsSnap is just a bridge — it hands off the PendingConnection to your wsHandler. 
  -- Everything else is up to you. Broadcast messages to all clients, Count or log active connections,
  -- Assign client IDs or session tokens, Or keep chat history...

-- | @type ServerApp = PendingConnection -> IO ()@ is a fucntion type, hence why `pending`
--   appears down here.
wsHandler :: TVar [NamedConn] -> WS.ServerApp
wsHandler tvConns pending = do
  let path = toString $ WS.requestPath $ WS.pendingRequest pending
  putStrLn $ "-------------->>>>>>>>>>>> " <> path
  let req = WS.pendingRequest pending
      path = WS.requestPath req
  putStrLn $ "Request path: " <> show path
  conn <- WS.acceptRequest pending
  forever $ do
    time <- getCurrentTime
    let strTime = pack $ show time
    conns <- liftIO $ atomically $ readTVar tvConns
    sequence_ $ WS.sendTextData conn <$> fst <$> conns -- [IO ()} 
    WS.sendTextData conn ( strTime :: Text)
    threadDelay 1000000
