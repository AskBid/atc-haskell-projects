{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

module LoginPage where

import Common.Route 
import Reflex.Dom.Core
-- import Reflex.Dom
import Common
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Frontend
import qualified Data.Text as T
import Data.Functor.Identity
import Common.Api (LoginReq(..))
import Control.Monad.Trans (lift)


loginPage :: ObeliskWidget t (R FrontendRoute) m  => AppState t -> RoutedT t () m ()
loginPage appState = do

  el "h2" $ text "Login here."
  el "label" $ text "Username: "
  usrEl <- inputElement $ def & initialAttributes .~ 
    ("class" =: "border")
  el "label" $ text "Password: "
  pwdEl <- inputElement $ def & initialAttributes .~ 
    ("class" =: "border" <> "type" =: "password")
  (btnEl, _) <- lift $ myButton "Submit"
  let submitClick = domEvent Click btnEl
  let usrDyn = _inputElement_value usrEl
  let pwdDyn = _inputElement_value pwdEl
  let logReqDyn = LoginReq <$> usrDyn <*> pwdDyn
  let loginReqEv = tagPromptlyDyn logReqDyn submitClick
  -- ^ loginReqEv, (postJson "text"), submitClick
  --         ==
  -- e l,        (l -> xhr),        e c 
  -- :: (l -> xhr) -> e l -> e xhr 
  -- (postJson "text") <$> e l :: e xhr 
  -- performRequestAsync       :: e xhr -> m (e xhr) 
   
  prerender (pure ()) $ do 
    let url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Tail_Login
    evResp <- performRequestAsync $ (postJson url) <$> loginReqEv

    let evLoginSuccess = ffilter (statusCheck 200 300) evResp
    setRoute $ FrontendRoute_Main :/ () <$ evLoginSuccess

    message <- holdDyn "Enter your credentials." $ 
      "Wrong credentials. Try again." <$ ffilter (not . statusCheck 200 300) evResp
    el "h4" $ dynText message

    return ()
   
  return ()

