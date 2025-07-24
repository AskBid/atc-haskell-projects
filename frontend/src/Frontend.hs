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

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Chat websocket Reflex"
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "div" $ text "Welcome to Chat! (from frontend)"
      subRoute_ $ \r -> case r of
        FrontendRoute_Main -> do
          el "h1" $ text "Main"
          
        FrontendRoute_User -> do
          dRoute <- askRoute
          el "h1" $ dynText dRoute
          prerender_ blank $ do
            
            el "lable" $ text "Connect w/ username:"
            elInp <- inputElement def
            (elBtn, _) <- el' "button" $ text "click"
            el "br" blank
            el "label" $ text "Message:"
            elInpMex <- inputElement def
            (elBtnMex, _) <- el' "button" $ text "click"

            let eClick = domEvent Click elBtn
                eSend = domEvent Click elBtnMex
                dInp = _inputElement_value elInp
                dMex = _inputElement_value elInpMex
                eName = tagPromptlyDyn dInp eClick
                eMex = tagPromptlyDyn dMex eClick
                eUrl = ("ws://localhost:8000/ws/" <>) <$> eName
            dName <- holdDyn "--" eName
            el "div" $ dynText dName
            
            let eSocket = ffor eUrl $ \url -> do 
                  liftIO $ putStrLn $ "Opening WebSocket at: " ++ T.unpack url
                  let wsConfig = def & webSocketConfig_reconnect .~ False
                                     & webSocketConfig_send .~ ((:[]) <$> eMex)
                  ws <- webSocket url wsConfig
                  let evText = ET.decodeUtf8 <$> _webSocket_recv ws
                  -- ^ Event keep on triggering when new message comes from WS backend
                  performEvent_ $ liftIO . putStrLn . T.unpack <$> evText
                  dText <- foldDyn foldyn "..." evText  
                  el "div" $ dynText dText

            widgetHold (el "div" $ text "nothing to see") eSocket

            return ()

      return ()
  }


foldyn :: T.Text -> T.Text -> T.Text
foldyn a b = a <> " " <> b
