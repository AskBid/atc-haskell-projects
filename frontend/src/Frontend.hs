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
            (elBtn, _) <- elClass' "button" buttonStyle $ text "click"
            let eClick = domEvent Click elBtn
                dInp = _inputElement_value elInp
                eName = tagPromptlyDyn dInp eClick
            setRoute $ (FrontendRoute_User :/ ) <$> eName
          
        FrontendRoute_User -> do
          prerender_ blank $ do  
            dName <- askRoute
            el "h1" $ dynText dName
            elClass "div" divVerticalStyle $ do

              ePostBuild <- getPostBuild
              let eName = tagPromptlyDyn dName ePostBuild
                  eUrl = ("ws://localhost:8000/ws/" <>) <$> eName

              let eSocket = ffor eUrl $ \url -> do 
                    elClass "label" labelStyle $ text "Message:"
                    elInpMess <- inputElement $ def & initialAttributes .~ ("class" =: inputStyle)
                    (elBtnSend, _) <- elClass' "button" buttonStyle $ text "Send >"

                    let eSend = domEvent Click elBtnSend
                        dMessText = _inputElement_value elInpMess
                        eMessText = tagPromptlyDyn dMessText eSend
                        eWSMess = NewMessage <$> mkMessage <$> eMessText 
                    
                    let wsConfig = def & webSocketConfig_reconnect .~ True
                                       & webSocketConfig_send .~ ((:[]) <$> A.encode <$> eWSMess)
                    liftIO $ putStrLn $ "Opening WebSocket at: " ++ T.unpack url
                    ws <- webSocket url wsConfig
                    -- ^ Event keep on triggering when new message comes from WS backend
                    let evmWSMessage = A.decode . BSL.fromStrict <$> _webSocket_recv ws

                    performEvent_ $ liftIO . putStrLn . (\m -> case m of 
                        Nothing -> "JSON invalid."
                        Just (NewMessage message) -> (T.unpack . messageText) message 
                        otehrwise -> "case TODO..." ) <$> evmWSMessage

                    dChatMessages <- foldDyn (:) [] $ feMessage <$> evmWSMessage 
                    
                    d <- holdDyn "no message yet." $ (\wsm -> case wsm of 
                           Just (NewMessage msg) -> messageText msg
                           otherwise -> "Different type of WS message received") <$> evmWSMessage

                    elClass "div" divVerticalStyle $ void $ do
                      simpleList dChatMessages $ \dMsg -> el "div" $ dynText dMsg

              widgetHold (el "div" $ text "No connection.") eSocket 

            return ()

      return ()
  }

mkMessage :: T.Text -> Message
mkMessage t = Message 
  { messageTimestamp = Nothing
  , messageText = t
  , messageOwner = Nothing
  , messagePrivate = Nothing
  }
