{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Common.Route
import Obelisk.Backend

import Obelisk.Route -- (R(..))
import Snap

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve backendHandlers
  , _backend_routeEncoder = fullRouteEncoder
  }

-- The serve function is provided by Obelisk. It is passed into _backend_run.
-- _backend_run = \serve -> serve backendHandlers
-- ...means:
-- “When the Obelisk backend server starts, use backendHandlers to respond to requests. serve will automatically use the route encoder to map URLs to BackendRoute constructors and pass the right one into backendHandlers.”
-- Sets up Snap (Obelisk uses Snap under the hood)
-- Uses `fullRouteEncoder` to decode the request path into a `BackendRoute`
-- Passes that `BackendRoute` to your handler: like `BackendRoute_Login :/ ()`
-- You respond with a Snap action


-- backendHandlers :: R BackendRoute -> Snap ()
-- This is the Obelisk-preferred typed route dispatching style:

backendHandlers :: R BackendRoute -> Snap ()
backendHandlers = \case
  BackendRoute_Login :/ () -> writeBS "login backend"
  BackendRoute_Logout :/ () -> writeBS "logout backend"
  BackendRoute_Missing :/ () -> writeBS "404 - Not Found"

-- `R` it’s the standard (advanced and complicated) way to refer to parsed routes in Obelisk.

-- :/ is a type-safe path separator
-- It separates a route constructor from its parameter(s) — 
-- think of it like a typed version of a slash (/) in a URL.

