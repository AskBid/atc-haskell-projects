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
            elInpName <- inputElement $ def 
              & initialAttributes .~ ("class" =: inputStyle)

            elClass "label" labelStyle $ text "Authenticate w/ password:"
            elInpPwd <- inputElement $ def 
              & initialAttributes .~ ("class" =: inputStyle <> "type" =: "password")
            
            (elBtn, _) <- elClass' "button" buttonStyle $ text "click"

            let eClick = domEvent Click elBtn
                dName = _inputElement_value elInpName
                dPwd = _inputElement_value elInpPwd
                dCredentials = Credentials <$> dName <*> dPwd
                eCredentials = tagPromptlyDyn dCredentials eClick
                reqLogin = \credentials -> xhrRequest "GET" "/login" $ def
                  & xhrRequestConfig_withCredentials .~ True
                  & xhrRequestConfig_headers .~ 
                    ("Authorization" =: (ET.decodeUtf8 $ BSL.toStrict $ A.encode credentials))

            prerender_ blank $ do 
              eXhrResp <- performRequestAsync $ reqLogin <$> eCredentials
              dStatusText <- holdDyn "noLog" $ T.pack . show . _xhrResponse_status <$> eXhrResp
              el "h1" $ dynText dStatusText
              return ()
              -- setRoute $ (FrontendRoute_User :/ ) <$> eName
          
        FrontendRoute_User -> userChat
      return ()
  }

