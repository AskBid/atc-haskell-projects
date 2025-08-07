{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE FlexibleContexts #-} 

module Common where

import Data.Text
import Reflex.Dom.Core
import Database.Persist.Class.PersistEntity (Entity)
import Obelisk.Route
import Data.Functor.Identity
import Language.Javascript.JSaddle (MonadJSM)
import qualified Data.Aeson as A (FromJSON, decode)
import qualified Data.ByteString.Lazy as BSL 
import qualified Data.Text.Encoding as ET (encodeUtf8)

import Common.Api
import Schema
import Common.Route

buttonStyle :: Text  
buttonStyle = "px-4 py-3 text-sm leading-tight bg-blue-600 text-white rounded hover:bg-blue-700"

inputStyle :: Text  
inputStyle = "border border-gray-300 rounded px-3 py-2"

labelStyle :: Text
labelStyle = "text-sm font-medium"

divVerticalStyle :: Text
divVerticalStyle = "flex flex-col gap-4 p-4"

divHorizontalStyle :: Text
divHorizontalStyle = "flex flex-row gap-4 p-4 items-center justify-center"

-- | translates websocket messages into Frontend messages, @Text@s ready for the chat.
feMessage :: Maybe WSMessage -> Text
feMessage wsm = case wsm of
  Nothing -> "*Non Valid Message*"
  Just m -> case m of
    NewMessage m -> "TODO: user> " <> messageText m
    ConnectedClients _ -> "TODO: List of connections message."
    otherwise -> "TODO: unknown message."

data AppState t = AppState 
  { authWSconn :: Maybe (WebSocket t)
  , unAuthWSconn :: Maybe (WebSocket t)
  , loggedAs :: Dynamic t (LoginState User)
  , loggedTrigger :: LoginState User -> IO ()
  }

data LoginState a = LoggedIn a | LoggedOut
  deriving (Show)

class LikeMaybe f where
  fromMaybe :: Maybe a -> f a

instance LikeMaybe LoginState where
  fromMaybe Nothing  = LoggedOut
  fromMaybe (Just a) = LoggedIn a

instance LikeMaybe Maybe where
  fromMaybe = id 

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
statusCheck min max xhr
  | status >= min && status < max = True
  | otherwise                     = False
  where status = _xhrResponse_status xhr

-- | handles the response from a given XhrRequest filtering for successul response
--   and decodes response using Aeson.FromJSON
decodeResponse :: (A.FromJSON a, Reflex t)
  =>  b -> Event t XhrResponse -> Event t (Either b (Maybe a))
decodeResponse err evRes = do 
  ffor evRes $ \res -> if statusCheck 200 300 res
    then Right $ decodeXhrResponse res
    else Left err
