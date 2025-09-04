{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Maybe as DM (fromMaybe)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom.Core
import Reflex.Dom.WebSocket (webSocket, WebSocketConfig(..), WebSocket)

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

      prerender_ blank $ mdo 
        ePostBuild <- getPostBuild
        -------------------------------------
        -- setting up Dynamic for LogingState
        --
        (evLoggedByTrigger, loggedTrigger') <- newTriggerEvent
        let route = FullRoute_Backend BackendRoute_Me :/ () 
        evLoggedMUser <- requestWithCredentialsAndDecode route ePostBuild
        let evLogged = leftmost [evLoggedMUser, evLoggedByTrigger]
        dLogged <- holdDyn Loading evLogged
        -- ^ without Maybe we would run straight into the redirection
        --   while requestWithCredentialsAndDecode is still running and the
        --   FRP diagram would flow in LoggedOut
        
        -------------------------------------
        -- setting up Event for send WebscoketState
        --
        (evWsSend', wsSendTrigger') <- newTriggerEvent

        let appState = AppState 
              { wsConn = dConn
              , evWsSend = evWsSend'
              , wsSendTrigger = wsSendTrigger'
              , loggedAs = dLogged
              , loggedTrigger = loggedTrigger'
              }
        
        ---------------------------------------------
        -- Dynamically setting the AppState WebSocket
        --
        -- dConn important to nnot end the ws cycle
        -- When your widget is torn down (because LoggedIn switched to 
        -- LoggedOut or you navigate away), Reflex disposes of the 
        -- webSocket resource. No neec to add a webSocket_close Event.
        dConn <- widgetHold (pure NoConnection) $ 
          ffor (updated (loggedAs appState)) $ \case
            LoggedIn u -> do
              let url = "ws://localhost:8000/ws/user/" <> _userName u
              ws <- webSocket url $ def
                      & webSocketConfig_reconnect .~ False
                      & webSocketConfig_send .~ ((:[]) <$> evWsSend appState)
                      -- ^ evWsSend will then be triggered in the interface location
                      --   where we need it.

              performEvent_ $ ffor (_webSocket_recv ws) $ \msg ->
                liftIO $ putStrLn ("WS recv (Auth): " <> show msg)

              pure (AuthConnection ws)

            LoggedOut -> do
              let url = "ws://localhost:8000/ws"
              ws <- webSocket url $ def
                      & webSocketConfig_reconnect .~ False
                      & webSocketConfig_send .~ ((:[]) <$> evWsSend appState)

              performEvent_ $ ffor (_webSocket_recv ws) $ \msg ->
                liftIO $ putStrLn ("WS recv (Public): " <> show msg)

              pure (PublicConnection ws)
        -- --
        -- Dynamically setting the AppState WebSocket
        ---------------------------------------------

        -----------
        -- Page
        elClass "div" "flex flex-col h-screen w-screen" $ do
          -----------
          -- HEADER 
          elClass "div" ("bg-white w-full " <> divHorizontalStyle) $ do

            dyn_ $ ffor (loggedAs appState) $ \case
              LoggedIn u  -> do 
                elClass "div" divHorizontalStyleNoGap $ do 
                  logoutButton appState 
                  let username = _userName u
                  let userUrl = getUrl (FullRoute_Frontend 
                                       (ObeliskRoute_App FrontendRoute_User) 
                                       :/ username)
                  elAttr "a" 
                    (  "class" =: ("px-4 " <> linkStyle) 
                    <> "href" =: userUrl
                    ) $ text username
              otherwise   -> el "div" $ text "Welcome to Chat!"
          -- HEADER
          -----------
          
          elClass "div" ("flex flex-1 w-full") $ do
            ------------
            -- CHAT/AUTH
            elClass "div" "flex-1 bg-gray-100 p-4" $ do
              liftIO $ putStrLn "inside Frontend just before setRoute..+++++"

              subRoute_ $ \case
                FrontendRoute_Main -> do 
                  dyn_ $ ffor (loggedAs appState) $ \case
                    Loading     -> el "div" $ text "Loading... MainRoute"
                    LoggedIn _  -> mainPageLogged appState
                    LoggedOut   -> mainPage appState
                FrontendRoute_User -> do 
                  dyn_ $ ffor (loggedAs appState) $ \case
                    Loading    -> el "div" $ text "Loading... UserRoute"
                    LoggedIn _ -> userChat appState
                    LoggedOut  -> mainPage appState
            -- CHAT/AUTH
            ------------

            -----------------------------
            -- CONNECTED CLIENTS SIDE BAR
            elClass "div" "w-[clamp(100px,20%,999px)] bg-gray-200 p-4" $ do 

              elClass "h1" "text-green-500 font-bold" $ text "Connected Users"
              elClass "div" divConnectedUsers $ do 
                dyn_ $ ffor (wsConn appState) $ \ws -> case getWS ws of
                  Nothing -> pure ()
                  Just ws -> do 
                    let eRecvRaw = _webSocket_recv ws
                        eWSMessage = DM.fromMaybe NoMessage 
                                     <$> A.decode . BSL.fromStrict 
                                     <$> eRecvRaw
                        eUsersConnected = flip fmapMaybe eWSMessage $ \case 
                          ConnectedClients users -> Just users 
                          otherwise              -> Nothing
                    void $ listUsers eUsersConnected 
            -- CONNECTED CLIENTS SIDE BAR
            -----------------------------
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
  => Event t [User] -> m (Dynamic t [()])
listUsers eNames = do 
  dUsrList <- holdDyn [] $ (_userName <$>) <$> eNames
  simpleList dUsrList (\dText -> el "div" $ dynText dText)

requestWithCredentialsAndDecode 
  :: ( MonadJSM (Performable m)
     , PerformEvent t m 
     , TriggerEvent t m 
     )
  => R (FullRoute BackendRoute FrontendRoute) 
  -> Event t a 
  -> m (Event t LoginState)
requestWithCredentialsAndDecode route event = do 
  let url = getUrl route
      req = xhrRequest "POST" url $ def 
        & xhrRequestConfig_withCredentials .~ True
  evRes <- performRequestAsync $ req <$ event
  let evEMDecoded = decodeResponse "Error." evRes
  pure $ ffor evEMDecoded $ \case
    Left _         -> LoggedOut
    Right Nothing  -> LoggedOut
    Right (Just u) -> LoggedIn u

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
  (elBtnLogout, _) <- elClass' "button" buttonStyle $ text "Disconnect/Logout"
  let eClickLogout = domEvent Click elBtnLogout 
      route = FullRoute_Backend BackendRoute_Logout :/ () 
  eUser <- requestWithCredentialsAndDecode route eClickLogout
  let evSucc = ffor eUser $ \case 
        LoggedOut           -> ()
        LoggedIn (User _ _ _) -> () 
  -- ^ I am here using User solely to be able to reuse @requestWithCredentialsAndDecode@
  --   but we are only interested that the events fires if the statusCheck was filtered
  performEvent_ $ (liftIO $ loggedTrigger appState $ LoggedOut) <$ evSucc
