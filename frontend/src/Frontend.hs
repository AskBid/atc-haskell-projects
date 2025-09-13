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

import Control.Monad
import Language.Javascript.JSaddle (MonadJSM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL 
import Control.Monad.Fix (MonadFix)
import qualified Data.Maybe as DM (fromMaybe)

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Common
import Database.Schema
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
        dLogged <- holdDyn LoggedOut evLogged
        -- ^ without Maybe we would run straight into the redirection
        --   while requestWithCredentialsAndDecode is still running and the
        --   FRP diagram would flow in LoggedOut
        
        -------------------------------------
        -- setting up Event for send WebscoketState
        --

        let appState = AppState 
              { wsStateDyn    = dConn
              , loggedAs      = dLogged
              , loggedTrigger = loggedTrigger'
              }
        
        ---------------------------------------------
        -- Dynamically setting the AppState WebSocket
        --
        -- dConn important to not end the ws cycle
        -- When your widget is torn down (because LoggedIn switched to 
        -- LoggedOut or you navigate away), Reflex disposes of the 
        -- webSocket resource. No need to add a webSocket_close Event.
        dConn <- widgetHold (pure NoConnection) $ 
          ffor (updated (loggedAs appState)) $ \case
            LoggedIn u -> do
              let url = getUrl $ FullRoute_Backend 
                                 BackendRoute_Websocket :/ 
                                 WebscocketRoute_User :/ (_userName u)
                  fullUrl = "ws://localhost:8000" <> url
                  -- TODO use Document.Local to find protocol and host address.
              (evSend, triggerSend)   <- newTriggerEvent
              (evClose, triggerClose) <- newTriggerEvent

              ws <- webSocket fullUrl $ def
                      & webSocketConfig_reconnect .~ False 
                      & webSocketConfig_send .~ ((:[]) <$> evSend)
                      -- ^ evWsSend will then be triggered in the interface location
                      --   where we need it.
                      & webSocketConfig_close .~ ((1000, "User closed WebSocket.") 
                        <$ evClose)

              let wsConnection = WSConfig
                    { wsConn         = ws
                    , wsSendTrigger  = triggerSend
                    , wsCloseTrigger = triggerClose
                    }
              -- TODO NOTE I havent tested throughtly but it seem that the performEvent
              -- below was the cause of a jacking of the send message that was appearing
              -- randomly. This recv may contribute to conentions situation. 
              -- Also possible I was just tricked into some external issue.
              --
              -- performEvent_ $ ffor (_webSocket_recv ws) $ \msg ->
              --   liftIO $ putStrLn (( T.unpack $ _userName u ) 
              --                     <> " FE: WS recv (Auth): " 
              --                     <> show msg ) 

              pure (AuthConnection wsConnection)


            LoggedOut -> do
              let url = getUrl $ FullRoute_Backend 
                                 BackendRoute_Websocket :/ 
                                 WebscocketRoute_Main :/ ()
                  fullUrl = "ws://localhost:8000" <> url
                  -- TODO use Document.Local to find protocol and host address.
              (evSend, triggerSend)   <- newTriggerEvent
              (evClose, triggerClose) <- newTriggerEvent

              ws <- webSocket fullUrl $ def
                      & webSocketConfig_reconnect .~ False
                      & webSocketConfig_send .~ ((:[]) <$> evSend)
                      & webSocketConfig_close .~ ((1000, "User closed WebSocket.") 
                        <$ evClose)

              let wsConnection = WSConfig
                    { wsConn         = ws
                    , wsSendTrigger  = triggerSend
                    , wsCloseTrigger = triggerClose
                    }

              -- performEvent_ $ ffor (_webSocket_recv ws) $ \msg ->
              --   liftIO $ putStrLn ("FE: WS recv (Public): " <> show msg)
              
              pure (PublicConnection wsConnection)
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
                  let username' = _userName u
                  let userUrl = getUrl (FullRoute_Frontend 
                                       (ObeliskRoute_App FrontendRoute_User) 
                                       :/ username')
                  elAttr "a" 
                    (  "class" =: ("px-4 " <> linkStyle) 
                    <> "href" =: userUrl
                    ) $ text username'

              _ -> el "div" $ text "Welcome to Chat!"
          -- HEADER
          -----------
          
          elClass "div" ("flex flex-1 w-full") $ do
            ------------
            -- CHAT/AUTH
            elClass "div" "flex-1 bg-gray-100 p-4" $ do
              liftIO $ putStrLn "FE: inside Frontend just before setRoute..+++++"

              subRoute_ $ \case
                FrontendRoute_Main -> do 
                  dyn_ $ ffor (loggedAs appState) $ \case
                    LoggedIn _  -> mainPageLogged 
                    LoggedOut   -> mainPage appState
                FrontendRoute_User -> do 
                  dyn_ $ ffor (loggedAs appState) $ \case
                    LoggedIn _ -> userChat appState
                    LoggedOut  -> mainPage appState
            -- CHAT/AUTH
            ------------

            -----------------------------
            -- CONNECTED CLIENTS SIDE BAR
            elClass "div" "w-[clamp(100px,20%,999px)] bg-gray-200 p-4" $ do 

              elClass "h1" "text-green-500 font-bold" $ text "Connected Users"
              elClass "div" divConnectedUsers $ do 
                dyn_ $ ffor (wsStateDyn appState) $ \ws -> case getWS ws of
                  Nothing  -> pure ()
                  Just ws' -> do 
                    let eRecvRaw = _webSocket_recv $ wsConn ws'
                        eWSMessage = DM.fromMaybe NoMessage 
                                     <$> A.decode . BSL.fromStrict 
                                     <$> eRecvRaw
                        eUsersConnected = flip fmapMaybe eWSMessage $ \case 
                          ConnectedClients users -> Just users 
                          _                      -> Nothing
                    void $ listUsers eUsersConnected 
            -- CONNECTED CLIENTS SIDE BAR
            -----------------------------
        return ()
      return ()
  }

-- | method used in the clients connected column bar to render the 
-- connected users
listUsers 
  :: ( MonadHold t m 
     , Reflex t
     , MonadFix m
     , PostBuild t m
     , DomBuilder t m 
     , RouteToUrl (R FrontendRoute) m 
     , SetRoute t (R FrontendRoute) m
     , Prerender t m
     ) 
  => Event t [User] -> m (Dynamic t [()])
listUsers eNames = do 
  dUserList <- holdDyn [] $ (_userName <$>) <$> eNames
  simpleList dUserList $ \dUsername -> 
    el "div" $ do
      dyn_ $ ffor dUsername $ \username' ->
        routeLink (FrontendRoute_User :/ username') $
          elAttr "div"
            ( "class" =:
              "block cursor-pointer py-0 p-1 \
              \bg-white hover:bg-fuchsia-400 hover:text-white \
              \transition-colors duration-100"
            ) $ text username'
 
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
  let evEMDecoded = decodeResponse ("Error." :: T.Text) evRes
  pure $ ffor evEMDecoded $ \case
    Left _         -> LoggedOut
    Right Nothing  -> LoggedOut
    Right (Just u) -> LoggedIn u

-- | Button used to when appState as logged in state to redirect to api/logout
logoutButton 
  :: ( DomBuilder t m 
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m 
     , PostBuild t m
     ) 
  => AppState t -> m ()
logoutButton appState = do 
  (elBtnLogout, _) <- elClass' "button" buttonStyle $ text "Disconnect/Logout"
  let eClickLogout = domEvent Click elBtnLogout 
      route = FullRoute_Backend BackendRoute_Logout :/ () 
  eUser <- requestWithCredentialsAndDecode route eClickLogout
  let evSucc = ffor eUser $ \case 
        LoggedOut             -> ()
        LoggedIn (User _ _ _) -> () 
  -- ^ I am here using User solely to be able to reuse @requestWithCredentialsAndDecode@
  --   but we are only interested that the events fires if the statusCheck was filtered
  performEvent_ $ (liftIO $ loggedTrigger appState $ LoggedOut) <$ evSucc
  dyn_ $ ffor (wsStateDyn appState) $ \wsState' -> 
    case getWS wsState' of 
      Nothing -> pure ()
      Just ws -> 
        performEvent_ $ (liftIO $ (wsCloseTrigger ws) $ ()) <$ evSucc
