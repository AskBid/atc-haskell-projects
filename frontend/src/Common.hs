{-# LANGUAGE OverloadedStrings #-} 

module Common where

import Data.Text
import Reflex.Dom.Core
import Database.Persist.Class.PersistEntity (Entity)
import Obelisk.Route
import Data.Functor.Identity

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
  , loggedAs :: Dynamic t (Maybe (Entity User))
  , guestAs :: Maybe Text
  }

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
getUrl route = intercalate "/" $ fst pageName
  where 
    pageName = encode safeEncoder route
