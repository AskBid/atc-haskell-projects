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
      el "div" $ text "Welcome to Chat!"
      subRoute_ $ \r -> case r of
        FrontendRoute_Main -> do
          elClass "div" divVerticalStyle $ do
            el "h1" $ text "MainPage"

            elClass "label" labelStyle $ text "Connect w/ username:"
            elInp <- inputElement $ def 
              & initialAttributes .~ ("class" =: inputStyle)

            elClass "label" labelStyle $ text "Connect w/ username:"
            elPwd <- inputElement $ def 
              & initialAttributes .~ ("class" =: inputStyle <> "type" =: "password")
            
            (elBtn, _) <- elClass' "button" buttonStyle $ text "click"

            let eClick = domEvent Click elBtn
                dInp = _inputElement_value elInp
                eName = tagPromptlyDyn dInp eClick

            let reqLogin = xhrRequest "GET" "/login" $ def 
                  & xhrRequestConfig_user .~ (Just "sergio")
                  & xhrRequestConfig_password .~ (Just "pwd")

            prerender_ blank $ do 
              performRequestAsync $ reqLogin <$ eClick 
              return ()
            setRoute $ (FrontendRoute_User :/ ) <$> eName
          
        FrontendRoute_User -> userChat
      return ()
  }

