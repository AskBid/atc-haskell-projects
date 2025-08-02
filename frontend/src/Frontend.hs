{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}

module Frontend where

import Control.Lens ((^.))
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as ET
import Language.Javascript.JSaddle (liftJSM, js, js1, jsg)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL 
import Control.Monad.Fix (MonadFix)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Common
import Schema
import UserChat
import MainPage


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Chat websocket Reflex"
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "script" ("src" =: "https://cdn.tailwindcss.com") blank
  , _frontend_body = do
      ePostBuild <- getPostBuild
      elClass "div" "flex w-full h-screen" $ do
        elClass "div" "flex-1 bg-gray-200" $ do 
          el "div" $ text "Welcome to Chat!"
          subRoute_ $ \case
            FrontendRoute_Main -> mainPage
            FrontendRoute_User -> userChat
        elClass "div" "w-[clamp(100px,15%,999px)] bg-gray-100 gap-2 p-2" $ do 
          elClass "h1" "text-green-500" $ text "Connected Users"
          elClass "div" divVerticalStyle $ do 
            listUsers ["sergio", "mario", "UnAuthUser_Phill"] ePostBuild
          elClass "h1" "text-red-500" $ text "Offline Users"
          elClass "div" divVerticalStyle $ do
            listUsers ["bob", "alice"] ePostBuild
      return ()
  }

listUsers 
  :: ( Monad m
     , MonadHold t m
     , Reflex t
     , MonadFix m
     , PostBuild t m
     , Adjustable t m
     , DomBuilder t m
     ) 
  => [T.Text] -> Event t a -> m (Dynamic t [()])
listUsers names event = do 
  dUsrList <- holdDyn [] $ names <$ event
  simpleList dUsrList (\dText -> el "div" $ dynText dText)
