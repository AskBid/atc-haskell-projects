{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE FlexibleContexts, FlexibleInstances, DerivingStrategies #-} 

module Common where

import Data.Text
import Reflex.Dom.Core
import Obelisk.Route
import Data.Functor.Identity
import qualified Data.Aeson as A (FromJSON)
import qualified Data.ByteString.Lazy as BSL 

import Database.Schema
import Common.Route

buttonPrivateStyle :: Text
buttonPrivateStyle = buttonNoColor <> " bg-fuchsia-600 hover:bg-fuchsia-700"

buttonStyle :: Text  
buttonStyle = buttonNoColor <> " bg-blue-600 hover:bg-blue-700"
  
buttonNoColor :: Text  
buttonNoColor = "px-4 py-3 text-sm leading-tight text-white rounded"

inputStyle :: Text  
inputStyle = "border border-gray-300 rounded px-3 py-2"

labelStyle :: Text
labelStyle = "text-sm font-medium"

divVerticalStyle :: Text
divVerticalStyle = "flex flex-col gap-4 p-4"

divHorizontalStyle :: Text
divHorizontalStyle = "flex flex-row gap-4 p-4 items-center justify-center"

divHorizontalStyleNoGap :: Text
divHorizontalStyleNoGap = "flex flex-row gap-0 p-0 items-center justify-center"

divConnectedUsers :: Text
divConnectedUsers = "flex flex-col gap-0 p-2"

linkStyle :: Text
linkStyle = "underline text-blue-600 hover:text-blue-800"

data AppState t = AppState 
  { wsConn :: Dynamic t (WebsocketState t)
  , evWsSend :: Event t BSL.ByteString
  , wsSendTrigger :: BSL.ByteString -> IO ()
  , evWsClose :: Event t ()
  , wsCloseTrigger :: () -> IO ()
  , loggedAs :: Dynamic t LoginState
  , loggedTrigger :: LoginState -> IO ()
  }

data LoginState 
  = LoggedIn User 
  | LoggedOut 
  | Loading
  deriving stock Show

data WebsocketState t
  = NoConnection 
  | PublicConnection (WebSocket t)
  | AuthConnection (WebSocket t)

-- | as in the case of the login page, we don't care of what type
--   of socket we are getting, we just need a socket, this gets rid
--   of the differentiation between the two.
getWS :: WebsocketState t -> Maybe (WebSocket t)
getWS = \case
  PublicConnection ws -> Just ws
  AuthConnection ws   -> Just ws
  NoConnection        -> Nothing

-- | @fullRouteEncoder@ has an `Either Text` as a first (check) argument
--   to make it Identity as required from the use of @encode@, we need first to pass
--   it under the check of @checkEncoder@ which gets rid off the uncertainty if we receive text
--   or not.
safeEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
safeEncoder = 
  case checkEncoder fullRouteEncoder of
    Left err  -> error $ "Encoder check failed in safeEncoder: " <> unpack err
    Right enc -> enc

-- | Given a FullRoute returns a Text Url
--  getUrl (FullRoute_Frontend (ObeliskRoute_App FrontendRoute_Login) :/ ())
--  getUrl $ FullRoute_Backend BackendRoute_Api :/ Tail_Login
getUrl :: R (FullRoute BackendRoute FrontendRoute) -> Text
getUrl = renderObeliskRoute safeEncoder

-- | check on the status of XhrResponses given a range of acceptability.
-- e.g.
-- >>> statusCheck 200 400 xhrResp
-- >>> True
statusCheck :: Word -> Word -> XhrResponse -> Bool
statusCheck min' max' xhr
  | status >= min' && status < max' = True
  | otherwise                       = False
  where status = _xhrResponse_status xhr

-- | handles the response from a given XhrRequest filtering for successul response
--   and decodes response using Aeson.FromJSON
decodeResponse :: (A.FromJSON a, Reflex t)
  =>  b -> Event t XhrResponse -> Event t (Either b (Maybe a))
decodeResponse err evRes = do 
  ffor evRes $ \res -> 
    if statusCheck 200 300 res
      then Right $ decodeXhrResponse res
      else Left err
