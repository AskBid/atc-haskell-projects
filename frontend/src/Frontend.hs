{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

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
      el "title" $ text "Reflex Chat"
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "div" $ text "Welcome to Chat! (from frontend)"

      prerender_ blank $ do

        -- TODO create element to enter text that when submitted through button, sends 
        -- the word to the backend
        (elInp, a) <- el' "input" $ text "mah"
        (elBtn, _) <- el' "button" $ text "click"
        let eClick = domEvent Click elBtn
        dyndyn <- holdDyn (T.pack "--") $ (T.pack "name") <$ eClick
        el "div" $ dynText dyndyn

        ws <- webSocket "http://localhost:8000/ws?name=" (def :: Reflex t => WebSocketConfig t T.Text)
        let evText = ET.decodeUtf8 <$> _webSocket_recv ws
        dText <- holdDyn "" evText
        el "div" $ dynText dText
        return ()

      return ()
  }
