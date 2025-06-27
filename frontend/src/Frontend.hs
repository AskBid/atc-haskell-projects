{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE FlexibleContexts         #-}

module Frontend where

import Control.Lens ((^.))
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import Language.Javascript.JSaddle (liftJSM, js, js1, jsg)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route -- (FrontendRoute(..), UserID(..), fullRouteEncoder)

import Obelisk.Route.Frontend
import Control.Monad.Trans (lift)
import Control.Lens (Identity (..))
import Data.Aeson (Value(..))
import Control.Monad.IO.Class --(liftIO)
import Language.Javascript.JSaddle (MonadJSM)





import Common
import LoginPage

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
      elClass "div" "grid grid-cols-3 min-h-screen" $ do
        
        elClass "div" "bg-gray-100" blank
        elClass "div" "bg-white flex flex-col p-4 space-y-4" $ do 
           
          (evLoggedInByTrigger, triggerLoggedIn) <- newTriggerEvent
          
          evMeLoggedIn <- prerender (pure never) $ do 
            postBuildEv <- getPostBuild
            let xhrRequest = XhrRequest { _xhrRequest_method = "GET"
                , _xhrRequest_url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Tail_Me 
                , _xhrRequest_config = def
                }
            respEv <- performRequestAsync $ xhrRequest <$ postBuildEv
            return $ ffilter (statusCheck 200 300) respEv

          let dynLoggedInEvStation = leftmost [ True <$ (switchDyn evMeLoggedIn)
                                              , True <$ evLoggedInByTrigger
                                              ]
          dynLoggedIn <- holdDyn False dynLoggedInEvStation

          let appState = AppState {loggedIn = dynLoggedIn, loggedUser = Nothing}

          subRoute_ $ \case
            FrontendRoute_Main -> do
              dyn_ $ buttonLogInOut <$> (loggedIn appState)
              el "h2" $ text "Welcome to My X!"
              area <- textAreaElement $ def & initialAttributes .~ 
                ("placeholder" =: "Write your X here ..." <> "class" =: "bg-blue-100 w-full p-2 rounded min-h-40")
              return ()
            FrontendRoute_Login -> loginPage appState
            FrontendRoute_Signup -> el "h2" $ text "Signup here."
            FrontendRoute_Profile -> do
              dynUserId <- askRoute
              el "h1" $ dynText $ fmap (\uid -> "Profile for " <> uid) dynUserId
              return ()
          return ()
          
        elClass "div" "bg-gray-100" blank
      return ()
  }

buttonLogInOut 
  :: (DomBuilder t m, SetRoute t (R FrontendRoute) m, MonadJSM (Performable m))
  => Bool -> RoutedT t a m ()
buttonLogInOut logBool = 
  if not logBool 
  then do 
    (btnInEl, _) <- myButton "Login"
    let loginClick = domEvent Click btnInEl
    setRoute $ FrontendRoute_Login :/ () <$ loginClick
  else do
    (btnOutEl, _) <- myButton "Logout"
    let logoutClick = domEvent Click btnOutEl
    let url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Tail_Logout
    let xhrReq = XhrRequest { 
        _xhrRequest_method = "GET"
      , _xhrRequest_url = url
      , _xhrRequest_config = def & xhrRequestConfig_withCredentials .~ True
      }
    respEv <- performRequestAsync $ xhrReq <$ logoutClick
    -- dynCurrentRoute <- askRoute
    setRoute $ FrontendRoute_Main :/ () <$ respEv 


--     • Couldn't match type ‘FrontendRoute’ with ‘BackendRoute’
--         arising from a functional dependency between constraints:
--           ‘SetRoute t (Data.Dependent.Sum.DSum BackendRoute Identity) m’
-- arising from a use of ‘setRoute’ at /home/marep/git/ATC/atc-haskell-projects/frontend/src/Frontend.hs:104:5-61
--           ‘SetRoute t (R FrontendRoute) m’
--             arising from the type signature for:
--             buttonLogInOut :: (DomBuilder t m, SetRoute t (R FrontendRoute) m) 
--                            => Bool
--                            -> R FrontendRoute
--                            -> RoutedT t a m () 
-- at /home/marep/git/ATC/atc-haskell-projects/frontend/src/Frontend.hs:(94,1)-(95,52)

