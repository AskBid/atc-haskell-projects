{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Snap

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve backendHandlers
  , _backend_routeEncoder = fullRouteEncoder
  }

backendHandlers :: R BackendRoute -> Snap ()
backendHandlers = \case
  BackendRoute_Missing :/ () -> writeBS "404"
  BackendRoute_Websocket :/ () -> writeBS "ciao"
