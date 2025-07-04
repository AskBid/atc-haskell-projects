{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Backend where

import Common.Route
import Common.Api (LoginReq(..))
import Obelisk.Backend
import MyJWT (verifyJWT, createJWT, mkJWTCookie, cookieLogout)
import Schema 
import Common.MyFunctions

import Obelisk.Route -- (R(..))
import Snap

import qualified Data.Aeson as A
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (fromMaybe)


backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      runSqlite myDB $ do 
        populateDB
        return ()
      serve backendHandlers
  , _backend_routeEncoder = fullRouteEncoder
  }
  -- ^ The serve function is provided by Obelisk. It is passed into _backend_run.
  -- _backend_run = \serve -> serve backendHandlers
  -- ...means:
  -- “When the Obelisk backend server starts, use backendHandlers to respond to requests. 
  -- serve will automatically use the route encoder to map URLs to BackendRoute constructors 
  -- and pass the right one into backendHandlers.”
  -- Sets up Snap (Obelisk uses Snap under the hood)
  -- Uses `fullRouteEncoder` to decode the request path into a `BackendRoute`
  -- Passes that `BackendRoute` to your handler: like `BackendRoute_Login :/ ()`
  -- You respond with a Snap action


-- backendHandlers :: R BackendRoute -> Snap ()
-- This is the Obelisk-preferred typed route dispatching style:

backendHandlers :: R BackendRoute -> Snap ()
backendHandlers = \case
   
  BackendRoute_Api :/ Api_Login -> do
    usrPwd <- readRequestBody 10000
    let maybeUsrPwd = (A.decode usrPwd) :: Maybe LoginReq
    case maybeUsrPwd of
      Nothing -> do
        modifyResponse $ setResponseStatus 401 "Unauthorized"
        writeLBS "{\"error\": \"No LoginReq\"}"
      Just loginReq -> do 
        user <- loginDB loginReq
        case user of
          Nothing -> do 
            modifyResponse $ setResponseStatus 401 "Unauthorized"
            writeBS $ TE.encodeUtf8 "Invalid credentials."
          Just user -> do
            let jwt = createJWT user
            modifyResponse $ setContentType "application/json"
            modifyResponse $ addResponseCookie $ mkJWTCookie jwt
            writeBS $ TE.encodeUtf8 $ (userName $ user) <> " logged in."
   
  BackendRoute_Api :/ Api_Logout -> do 
    modifyResponse $ setContentType "application/json"
    let expiredJWTCookie = cookieLogout
    modifyResponse $ addResponseCookie $ expiredJWTCookie
    writeBS "logout backend. You shouldn't be here!"
   
  BackendRoute_Api :/ Api_Me -> do
    mUsername <- verifyJWT
    case mUsername of
      Nothing -> do
        modifyResponse $ setResponseStatus 401 "unauthorized"
        writeBS "invalid JWT"
      Just username -> do
        modifyResponse $ setResponseStatus 200 "OK"
        writeBS $ TE.encodeUtf8 username
   
  BackendRoute_Api :/ Api_Posts -> do
    posts <- liftIO $ getPosts 
    writeBS $ "all the primary tweets (not replies)"
    writeLBS $ A.encode $ entityVal <$> posts
   
  BackendRoute_Missing :/ () -> writeBS "404 - Not Found"
  -- ^ `R` it’s the standard (advanced and complicated) way to refer to parsed routes in Obelisk.
  -- :/ is a type-safe path separator
  -- It separates a route constructor from its parameter(s) — 
  -- think of it like a typed version of a slash (/) in a URL.


