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
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity
import Common.Api (LoginReq(..))
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Maybe (isJust)


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
    let url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Api_Login
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
        _ <- loginTrigger appState $ mEUser
            -- let evLoginTriggerIO = loginTriggerIO <$ evLoginSuccess -- :: Event t (IO ()) 
            -- performEvent_ $ liftIO <$> evLoginTriggerIO
        return ()
    setRoute $ FrontendRoute_Main :/ () <$ ffilter isJust evMTextResp
    return () 
  return ()

