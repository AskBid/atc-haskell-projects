{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo         #-}

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
import Control.Monad.Trans (lift)
import Control.Lens (Identity (..))
import Data.Aeson (Value(..))

import Common


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
          subRoute_ $ \case
            FrontendRoute_Main -> do
              (btnEl, _) <- lift $ myButton "Login"
              let loginClick = domEvent Click btnEl
              setRoute $ (FrontendRoute_Login :/ ()) <$ loginClick
              el "h2" $ text "Welcome to My X!"
              area <- textAreaElement $ def
                & initialAttributes .~ ("placeholder" =: "Write your X here ..." <> "class" =: "bg-blue-100 w-full p-2 rounded min-h-40")
              return ()
            FrontendRoute_Login -> do
              el "h2" $ text "Login here."
              el "label" $ text "Username: "
              usrEl <- inputElement $ def & initialAttributes .~ ("class" =: "border")
              el "label" $ text "Password: "
              pwdEl <- inputElement $ def & initialAttributes .~ ("class" =: "border" <> "type" =: "password")
              (btnEl, _) <- lift $ myButton "Submit"
              let submitClick = domEvent Click btnEl
              let usrDyn = _inputElement_value usrEl
              let pwdDyn = _inputElement_value pwdEl
              let logReqDyn = LoginReq <$> usrDyn <*> pwdDyn
              prerender (pure ()) $ do 
                let url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Tail_Login
                let loginReqEv = tagPromptlyDyn logReqDyn submitClick
                -- loginReqEv, (postJson "text"), submitClick
                --         ==
                -- e l,        (l -> xhr),        e c 
                -- :: (l -> xhr) -> e l -> e xhr 
                -- (postJson "text") <$> e l :: e xhr 
                -- performRequestAsync       :: e xhr -> m (e xhr) 
                respEv <- (performRequestAsync $ (postJson url) <$> loginReqEv)
                let maybetextResp = _xhrResponse_response <$> respEv
                let resp = (maybe (XhrResponseBody_Default "error") id <$> maybetextResp) 
                dynResp <- (holdDyn (XhrResponseBody_Default "error-2") resp)
                el "br" blank
                el "br" blank
                el "h1" $ text "----->>>  repsonse: "
                el "h3" $ dynText $ (\xhrRB -> case xhrRB of
                  XhrResponseBody_Default _     -> "XhrResponseBody_Default"
                  XhrResponseBody_Text text     -> text
                  XhrResponseBody_Blob _        -> "XhrResponseBody_Blob"
                  XhrResponseBody_ArrayBuffer _ -> "XhrResponseBody_ArrayBuffer"
                  otherwise                     -> "others") <$> dynResp
                return ()
              return ()
            FrontendRoute_Signup -> el "h2" $ text "Signup here."
            FrontendRoute_Profile -> do
              dynUserId <- askRoute
              el "h1" $ dynText $ fmap (\uid -> "Profile for " <> uid) dynUserId
              return ()
          return ()
        elClass "div" "bg-gray-100" blank
      return ()
  }

-- | @fullRouteEncoder@ has an `Either Text` as a first (check) argument
--   to make it Identity as required from the use of @encode@, we need first to pass
--   it under the check of @checkEncoder@ which gets rid off the uncertainty if we receive text
--   or not.
safeEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
safeEncoder = 
  case checkEncoder fullRouteEncoder of
    Left x    -> undefined
    Right enc -> enc

-- | Given a FullRoute returns a Text Url
--  getUrl (FullRoute_Frontend (ObeliskRoute_App FrontendRoute_Login) :/ ())
--  getUrl $ FullRoute_Backend BackendRoute_Api :/ Tail_Login
getUrl :: R (FullRoute BackendRoute FrontendRoute) -> T.Text
getUrl route = T.intercalate "/" $ fst pageName
  where 
    pageName = encode safeEncoder route
