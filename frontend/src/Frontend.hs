{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE FlexibleContexts      #-}

module Frontend where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Route -- (FrontendRoute(..), UserID(..), fullRouteEncoder)

import Obelisk.Route.Frontend
import Language.Javascript.JSaddle (MonadJSM)
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Database.Persist

import Common
import LoginPage
import MainPage
import Schema

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
      elClass "div" "grid grid-cols-[1fr_auto_1fr] min-h-screen" $ do
        
        elClass "div" "bg-gray-100" blank
        elClass "div" "min-w-[600px] max-w-[600px] w-full bg-white flex flex-col p-4 space-y-4" $ do 
           
          (evLoggedInByTrigger, loginTrigger) <- newTriggerEvent
          
          dMEUser <- prerender (pure never) $ do
            postBuildEv <- getPostBuild
            evMEUser <- meRouteLoginCheck postBuildEv
            return evMEUser

          let dynLoggedInEvStation = leftmost [ switchDyn dMEUser
                                              , evLoggedInByTrigger
                                              ]
          dynLoggedIn <- holdDyn Nothing dynLoggedInEvStation
          -- ^ this way we make the Dynamic that holds the login state to be dependent
          --   on more than just one Event.
          let appState = AppState {
              loggedUser = dynLoggedIn
            , loginTrigger = loginTrigger
            }

          let eUserName :: Maybe (Entity User) -> T.Text
              eUserName Nothing      = "No user is logged in."
              eUserName (Just eUser) = "Hello " <> (userName $ entityVal eUser) <> "!"
          elClass "h3" "font-bold text-gray-400" $ dynText (eUserName <$> loggedUser appState) 

          subRoute_ $ \case
            FrontendRoute_Main -> mainPage appState
            FrontendRoute_Login -> loginPage appState
            FrontendRoute_Signup -> el "h2" $ text "Signup here."
            FrontendRoute_Profile -> do
              dynUserId <- askRoute
              el "h1" $ dynText $ fmap (\uid -> "Profile for " <> uid) dynUserId
              return ()
            FrontendRoute_Tweet -> do 
              dynTweetId <- askRoute
              el "h1" $ dynText $ fmap (\uid -> "Tweet id is: " <> uid) dynTweetId
              return ()
          return ()
          
        elClass "div" "bg-gray-100" blank
      return ()
  }

-- | send XhrRequest to /me route to check if user is sending a valid JWT in the 
--   headers/set-cookies.
meRouteLoginCheck
  :: ( Monad m
     , MonadJSM (Performable m)
     , Reflex t 
     , PerformEvent t m
     , TriggerEvent t m
     ) 
  => Event t a -> m (Event t (Maybe (Entity User)))
meRouteLoginCheck evTrigger = do
  let xhrRequest = XhrRequest { 
      _xhrRequest_method = "GET"
    , _xhrRequest_url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Api_Me 
    , _xhrRequest_config = def
    }
  evResp <- performRequestAsync $ xhrRequest <$ evTrigger
  let evRespSucc = ffilter (statusCheck 200 300) evResp
      evMTextResp = _xhrResponse_responseText <$> evRespSucc
      evMEUser = ffor evMTextResp $ 
        \mTextResp -> case mTextResp of 
          Nothing -> Nothing 
          Just textResp -> (A.decode . BL.fromStrict . TE.encodeUtf8) textResp 
  return evMEUser
