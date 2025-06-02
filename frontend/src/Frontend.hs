{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend where

import Control.Lens ((^.))
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (liftJSM, js, js1, jsg)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route -- (FrontendRoute(..), UserID(..), fullRouteEncoder)

import Obelisk.Route.Frontend


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "My X"
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "script" ("src" =: "https://cdn.tailwindcss.com") blank
  , _frontend_body = do
      subRoute_ $ \case
        FrontendRoute_Main -> do
          elClass "div" "grid grid-cols-3 min-h-screen" $ do
            elClass "div" "bg-gray-100" blank

            elClass "div" "bg-white flex flex-col p-4 space-y-4" $ do 
              el "h2" $ text "Welcome to My X!"
              area <- textAreaElement $ def
                & initialAttributes .~ ("placeholder" =: "Write your X here ..." <> "class" =: "bg-blue-100  w-full p-2 rounded min-h-40")
              return ()

            elClass "div" "bg-gray-100" blank
          return ()
        FrontendRoute_Login -> el "h2" $ text "Login here."
        FrontendRoute_Signup -> el "h2" $ text "Signup here."
        FrontendRoute_Profile -> do
          dynUserId <- askRoute
          el "h1" $ dynText $ fmap (\uid -> "Profile for " <> uid) dynUserId

      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      -- prerender_ blank $ liftJSM $ void
      --   $ jsg ("window" :: T.Text)
      --   ^. js ("skeleton_lib" :: T.Text)
      --   ^. js1 ("log" :: T.Text) ("Hello, World!" :: T.Text)

      -- el "div" $ do
      --   let
      --     cfg = "common/example"
      --     path = "config/" <> cfg
      --   getConfig cfg >>= \case
      --     Nothing -> text $ "No config file found in " <> path
      --     Just bytes -> case T.decodeUtf8' bytes of
      --       Left ue -> text $ "Couldn't decode " <> path <> " : " <> T.pack (show ue)
      --       Right s -> text s
      return ()
  }
