{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend where

import Control.Lens ((^.), Identity)
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as ET
import Language.Javascript.JSaddle (liftJSM, js, js1, jsg, MonadJSM)
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
import Database.Persist.Class.PersistEntity (Entity)

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

      prerender_ blank $ do
        (evLoggedByTrigger, loggedTrigger) <- newTriggerEvent
        ePostBuild <- getPostBuild
        let route = FullRoute_Backend BackendRoute_Me :/ () 
        evLoggedMUser <- requestWithCredentialsAndDecode route ePostBuild
        let evLogged = leftmost [evLoggedMUser, evLoggedByTrigger]
        dLogged <- holdDyn LoggedOut evLogged
        
        let appState = AppState 
              { authWSconn = Nothing
              , unAuthWSconn = Nothing
              , loggedAs = dLogged
              , loggedTrigger = loggedTrigger
              }

        elClass "div" "flex flex-col h-screen w-screen" $ do
          elClass "div" ("bg-white w-full p-0 " <> divHorizontalStyle) $ do
            dyn_ $ ffor (loggedAs appState) $ \case
              LoggedIn _  -> logoutButton appState 
              LoggedOut -> do 
                el "div" $ text "Welcome to Chat!"
                elAttr "a" ("href" =: "/signup" <> "class" =: "text-blue-500 underline") $ text "Signup"

          elClass "div" ("flex flex-1 w-full") $ do
            elClass "div" "flex-1 bg-gray-100 p-4" $ do
              subRoute_ $ \case
                FrontendRoute_Main -> do 
                  dyn_ $ ffor (loggedAs appState) $ \case
                    LoggedOut -> mainPage appState
                    LoggedIn _  -> mainPageLogged appState
                FrontendRoute_User -> userChat appState

            elClass "div" "w-[clamp(100px,20%,999px)] bg-gray-200 p-4" $ do 

              elClass "h1" "text-green-500 font-bold" $ text "Connected Users"
              elClass "div" divVerticalStyle $ do 
                listUsers ["sergio", "mario"] ePostBuild

              elClass "h1" "text-red-500 font-bold" $ text "Offline Users"
              elClass "div" divVerticalStyle $ do
                listUsers ["bob", "alice"] ePostBuild
        return ()
      return ()
  }

listUsers 
  :: ( Monad m
     , MonadHold t m
     , Reflex t
     , MonadFix m
     , PostBuild t m
     , Adjustable t m
     , DomBuilder t m ) 
  => [T.Text] -> Event t a -> m (Dynamic t [()])
listUsers names event = do 
  dUsrList <- holdDyn [] $ names <$ event
  simpleList dUsrList (\dText -> el "div" $ dynText dText)

requestWithCredentialsAndDecode 
  :: ( MonadJSM (Performable m)
     , PerformEvent t m 
     , TriggerEvent t m 
     , A.FromJSON b
     )
  => R (FullRoute BackendRoute FrontendRoute) -> Event t a -> m (Event t (LoginState b))
requestWithCredentialsAndDecode route event = do 
  let url = getUrl route
      req = xhrRequest "POST" url $ def & xhrRequestConfig_withCredentials .~ True
  evRes <- performRequestAsync $ req <$ event
  let evEMDecoded = decodeResponse "Error." evRes
  pure $ fromMaybe <$> (fmapMaybe (either (const Nothing) id)) evEMDecoded

logoutButton 
  :: ( Monad m
     , DomBuilder t m 
     , MonadJSM (Performable m)
     , MonadJSM m 
     , PerformEvent t m
     , TriggerEvent t m 
     ) 
  => AppState t -> m ()
logoutButton appState = do 
  (elBtnLogout, _) <- elClass' "button" buttonStyle $ text "Logout"
  let eClickLogout = domEvent Click elBtnLogout 
      route = FullRoute_Backend BackendRoute_Logout :/ () 
  evMUser <- requestWithCredentialsAndDecode route eClickLogout
  let evSucc = ffor evMUser $ \case 
        LoggedOut           -> ()
        LoggedIn (User _ _) -> () 
  -- ^ I am here using User solely to be able to reuse @requestWithCredentialsAndDecode@
  --   but we are only interested that the events fires if the statusCheck was filtered
  performEvent_ $ (liftIO $ loggedTrigger appState $ LoggedOut) <$ evSucc
