{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

module SignupPage where

import Common.Route 
import Reflex.Dom.Core
-- import Reflex.Dom
import Common
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Frontend
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity
import Common.Api (LoginReq(..))
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Maybe (isJust)


signupPage :: ObeliskWidget t (R FrontendRoute) m  => AppState t -> RoutedT t () m ()
signupPage appState = do

  el "h2" $ text "Signup here."
  el "label" $ text "Username: "
  usrEl <- inputElement $ def & initialAttributes .~ 
    ("class" =: "border")
  el "label" $ text "Password: "
  pwdEl <- inputElement $ def & initialAttributes .~ 
    ("class" =: "border" <> "type" =: "password")
  (btnEl, _) <- lift $ myButton "Signup"
  let submitClick = domEvent Click btnEl
      usrDyn = _inputElement_value usrEl
      pwdDyn = _inputElement_value pwdEl
      logReqDyn = LoginReq <$> usrDyn <*> pwdDyn
      loginReqEv = tagPromptlyDyn logReqDyn submitClick

  prerender (pure ()) $ do 
    let url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Api_Signup
    evResp <- performRequestAsync $ (postJson url) <$> loginReqEv

    message <- holdDyn "Enter your credentials." $ 
      "Wrong credentials. Try again." <$ ffilter (not . statusCheck 200 300) evResp
    el "h4" $ dynText message

    let evRespLoginSucc = ffilter (statusCheck 200 300) evResp
        evMTextResp = _xhrResponse_responseText <$> evRespLoginSucc

    performEvent_ $ ffor evMTextResp $ \mTextResp -> case mTextResp of 
    -- ^ if you have an event with a monad you need to run, usually you need performEvent.
    --   when the event fires, runs the monad and returns an Event with its result.
      Nothing -> pure ()
      Just textEUser -> do 
        let mEUser = (A.decode . BL.fromStrict . TE.encodeUtf8) textEUser
        liftIO $ loginTrigger appState $ mEUser
        return ()

    setRoute $ FrontendRoute_Login :/ () <$ ffilter isJust evMTextResp
    return () 

  return ()

