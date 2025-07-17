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

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve backendHandlers
  , _backend_routeEncoder = fullRouteEncoder
  }

backendHandlers :: R BackendRoute -> Snap ()
backendHandlers = \case
  BackendRoute_Missing :/ () -> writeBS "404"
  BackendRoute_Websocket :/ () -> do 
    conns <- liftIO $ atomically $ newTVar []
    WSSnap.runWebSocketsSnap $ wsHandler conns
  -- ^ runWebSocketsSnap is just a bridge — it hands off the PendingConnection to your wsHandler. 
  -- Everything else is up to you. Broadcast messages to all clients, Count or log active connections,
  -- Assign client IDs or session tokens, Or keep chat history...

-- | @type ServerApp = PendingConnection -> IO ()@ is a fucntion type, hence why `pending`
--   appears down here.
wsHandler :: TVar [WS.Connection] -> WS.ServerApp
wsHandler conns pending = do
  conn <- WS.acceptRequest pending
  forever $ do
    time <- getCurrentTime
    let strTime = pack $ show time
    WS.sendTextData conn ( strTime :: Text)
    threadDelay 1000000
