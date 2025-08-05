{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-} 

module MainPage where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as ET
import Reflex.Dom.Core
import Obelisk.Route
import Obelisk.Route.Frontend
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

import Common
import Common.Api
import Schema
import Common.Route

mainPage 
  :: ( Prerender t m
     , Monad m
     , Routed t () m
     , Routed t () (Client m)
     , DomSpace (DomBuilderSpace m)
     , DomBuilder t m
     ) 
  => AppState t -> m ()
mainPage appState = do
  prerender_ blank $ do 
    elClass "div" divVerticalStyle $ do

      elClass "label" labelStyle $ text "Connect w/ username:"
      elInpName <- inputElement $ def 
        & initialAttributes .~ ("class" =: inputStyle)

      elClass "label" labelStyle $ text "Authenticate w/ password:"
      elInpPwd <- inputElement $ def 
        & initialAttributes .~ ("class" =: inputStyle <> "type" =: "password")
      
      (elBtn, _) <- elClass' "button" buttonStyle $ text "Connect"

      let eClick = domEvent Click elBtn
          dName = _inputElement_value elInpName
          dPwd = _inputElement_value elInpPwd
          dCredentials = Credentials <$> dName <*> dPwd
          eCredentials = tagPromptlyDyn dCredentials eClick
          url = getUrl $ FullRoute_Frontend (ObeliskRoute_App FrontendRoute_Main) :/ () 
          reqLogin = \credentials -> xhrRequest "GET" url $ def
            & xhrRequestConfig_withCredentials .~ True
            & xhrRequestConfig_headers .~ 
              ("Authorization" =: (ET.decodeUtf8 $ BSL.toStrict $ A.encode credentials))

      eXhrResp <- performRequestAsync $ reqLogin <$> eCredentials
      dStatusText <- holdDyn "noLog" $ T.pack . show . _xhrResponse_status <$> eXhrResp
      el "h1" $ dynText dStatusText
      return ()
        -- setRoute $ (FrontendRoute_User :/ ) <$> eName
