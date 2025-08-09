{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-} 
{-# LANGUAGE LambdaCase        #-} 
{-# LANGUAGE RecursiveDo       #-}

module MainPage where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as ET
import Reflex.Dom.Core
import Obelisk.Route
import Obelisk.Route.Frontend
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, join)
import qualified Data.Maybe as DM (isJust, fromMaybe)
import Data.Map (fromList, Map)
import Control.Monad.Fix

import Common
import Common.Api
import Schema
import Common.Route


mainPageLogged 
  :: ( Prerender t m
     , Monad m
     , Routed t () m
     , Routed t () (Client m)
     , DomSpace (DomBuilderSpace m)
     , DomBuilder t m
     , SetRoute t (R FrontendRoute) (Client m)
     ) 
  => AppState t -> m ()
mainPageLogged appState = do
  el "div" $ text "you already logged in."
  el "div" $ text "TODO: link to logout" 
  el "div" $ text "TODO: link to user's chat"

mainPage 
  :: ( Prerender t m
     , Monad m
     , Routed t () m
     , Routed t () (Client m)
     , DomSpace (DomBuilderSpace m)
     , DomBuilder t m
     , SetRoute t (R FrontendRoute) (Client m)
     , MonadFix m
     ) 
  => AppState t -> m ()
mainPage appState = do
  prerender_ blank $ mdo
    elClass "div" divVerticalStyle $ do

      elClass "label" labelStyle $ text "Connect w/ username:"
      elInpName <- inputElement $ def 
        & initialAttributes .~ ("class" =: inputStyle)
      let eInput = domEvent Input elInpName
          dName = _inputElement_value elInpName 
      eDebounced <- debounce 0.8 eInput
      let eName = tagPromptlyDyn dName eDebounced
      
      ws <- webSocket "ws://localhost:8000/ws" $ def 
              & webSocketConfig_reconnect .~ False
              & webSocketConfig_send .~ ((:[]) <$> eName)
      let evMWSMessage = A.decode . BSL.fromStrict <$> _webSocket_recv ws
      dWSMessage <- holdDyn NoUser $ DM.fromMaybe NoUser <$> evMWSMessage 

      let eUserExistence = ffor evMWSMessage $ 
            \case 
              Just (UserExist name) -> "user exist."
              otherwise             -> "no user, want to create?"

      elClass "label" labelStyle $ text "Authenticate w/ password:"
      elInpPwd <- inputElement $ def 
        & initialAttributes .~ ("class" =: inputStyle <> "type" =: "password")
      
      let dNameLenght = ffor dName $ \name -> T.length name > 2
          dLoginConditions = fmap and $ sequence 
            [ dNameLenght
            , join $ ffor dWSMessage $ wsMessage2equalName dName
            ]
          dSignupConditions = fmap and $ sequence
            [ dNameLenght
            , not <$> (join $ ffor dWSMessage $ (wsMessage2equalName dName))
            ]

      let dPwd = _inputElement_value elInpPwd
                
      dresult <- holdDyn "Enter your credentials or input new ones to signup." eUserExistence
      elClass "div" "flex items-center justify-center" $ dynText dresult

      (elBtn, _) <- elDynAttr' "button" 
        (toggleEnableStyle dLoginConditions) $ text "Connect"
      (elBtnSignUp, _) <- elDynAttr' "button" 
        (toggleEnableStyle dSignupConditions) $ text "Sign Up"

      let eClick = domEvent Click elBtn
          dCredentials = Credentials <$> dName <*> dPwd
          eCredentials = tagPromptlyDyn dCredentials eClick

          url = getUrl $ FullRoute_Backend BackendRoute_Login :/ ()

          reqLogin = \credentials -> xhrRequest "POST" url $ def
            & xhrRequestConfig_withCredentials .~ True
            & xhrRequestConfig_headers .~ 
              ("Authorization" =: (ET.decodeUtf8 $ BSL.toStrict $ A.encode credentials))

      evRes <- performRequestAsync $ reqLogin <$> eCredentials

      let evEitherResp = decodeResponse "Invalid username or password." evRes
          evErr = either id (const "Login success.") <$> evEitherResp
          evOkUsr = fmapMaybe (either (const Nothing) id) evEitherResp
      dErr <- holdDyn "" evErr
      el "div" $ dynText $ dErr
      performEvent_ $ liftIO . loggedTrigger appState <$> LoggedIn <$> evOkUsr
      setRoute $ (FrontendRoute_User :/ ) <$> userName <$> evOkUsr

buttonStyleDisabled :: T.Text
buttonStyleDisabled = buttonStyle <> " disabled:bg-grey-200 disabled:opacity-50 disabled:border-grey-300"

wsMessage2equalName 
  :: (Reflex t, Functor (Dynamic t)) 
  => Dynamic t T.Text -> WSMessage -> Dynamic t Bool
wsMessage2equalName dName wsm = 
  case wsm of
    UserExist nameuser -> (nameuser ==) <$> dName
    otherwise          -> False <$ dName

toggleEnableStyle 
  :: Functor (Dynamic t)
  => Dynamic t Bool -> Dynamic t (Map T.Text T.Text)
toggleEnableStyle dConditions = ffor dConditions $ 
  \enabled ->
    if enabled
      then (fromList [("class", buttonStyle)])
      else (fromList [("class", buttonStyleDisabled), ("disabled","")])
