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

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve backendHandlers
  , _backend_routeEncoder = fullRouteEncoder
  }

backendHandlers :: R BackendRoute -> Snap ()
backendHandlers = \case
  BackendRoute_Missing :/ () -> writeBS "404"
  BackendRoute_Websocket :/ () -> WSSnap.runWebSocketsSnap wsHandler

-- | @type ServerApp = PendingConnection -> IO ()@ is a fucntion type, hence why `pending`
--   appears down here.
wsHandler :: WS.ServerApp
wsHandler pending = do
  conn <- WS.acceptRequest pending
  WS.sendTextData conn ("WebSocket connected!" :: Text)
