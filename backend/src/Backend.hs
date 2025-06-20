{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Backend where

import Common.Route
import Common.Api (LoginReq(..))
import Obelisk.Backend

import Obelisk.Route -- (R(..))
import Snap
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

import qualified Data.Aeson as A
-- import qualified Data.Aeson.Types as A
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Web.JWT
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Schema 
import Control.Monad.IO.Class --(liftIO)
import Common.MyFunctions
import Data.Time.Clock (getCurrentTime)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      runSqlite "Xs.db" $ do 
      -- ^ this makes it eprsistent, use :memory: instead of Xs.db otherwise 
        runMigration migrateAll
        _ <- insertBy $ User "alice" "alice123" []
        _ <- insertBy $ User "bob" "bob456" []
        _ <- insertBy $ User "sergio" "pwd" []
        return ()
      serve backendHandlers
  , _backend_routeEncoder = fullRouteEncoder
  }

-- The serve function is provided by Obelisk. It is passed into _backend_run.
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
  BackendRoute_Api :/ Tail_Login -> do
    usrPwd <- readRequestBody 10000
    let maybeUsrPwd = (A.decode usrPwd) :: Maybe LoginReq
    case maybeUsrPwd of
      Nothing -> do
        -- modifyResponse $ setResponseStatus 401 "Unauthorized"
        writeLBS "{\"error\": \"Invalid credentials\"}"
      Just loginReq -> do 
        user <- loginDB loginReq
        json <- liftIO $ jwtIt user
        modifyResponse $ setContentType "application/json"
        -- ^ Not really necessary but It changes the HTTP response headers that
        --   the Snap backend sends back to the browser.
        writeBS $ BL.toStrict json
  -- BackendRoute_Logout :/ () -> writeBS "logout backend"
  BackendRoute_Missing :/ () -> writeBS "404 - Not Found"

-- `R` it’s the standard (advanced and complicated) way to refer to parsed routes in Obelisk.

-- :/ is a type-safe path separator
-- It separates a route constructor from its parameter(s) — 
-- think of it like a typed version of a slash (/) in a URL.

jwtIt :: Maybe User -> IO BL.ByteString
jwtIt Nothing = pure $ BSC.pack "{error: \"no user found.\"}"
jwtIt (Just user) = do
  now <- getCurrentTime
  let expTime = JWT.numericDate $ addUTCTime 3600 now
  let claims = JWT.ClaimsSet
    { JWT.iss = Nothing
    , JWT.sub = Just "user@example.com"
    , JWT.aud = Nothing
    , JWT.exp = expTime
    , JWT.nbf = Nothing
    , JWT.iat = Nothing
    , JWT.jti = Nothing
    , JWT.unregisteredClaims = mempty
    }
  return $ A.encode $ A.toJSON user


loginDB :: MonadIO m => LoginReq -> m (Maybe User)
loginDB lr = do
  liftIO $ runSqlite "Xs.db" $ do
    user <- selectList [UserName ==. (username lr), UserPwd ==. (password lr)] []
    return $ entityVal <$> headSafe user
  where 
    name = username lr 
    pwd = password lr
