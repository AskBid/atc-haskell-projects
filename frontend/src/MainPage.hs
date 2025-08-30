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
import Language.Javascript.JSaddle (MonadJSM)

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
  prerender_ blank $ do
    elClass "div" divVerticalStyle $ mdo

      elClass "label" labelStyle $ text "Username:"
      elInpName <- inputElement $ def 
        & initialAttributes .~ ("class" =: inputStyle)
        & inputElementConfig_setValue .~ evEmptyInput

      elClass "label" labelStyle $ text "Password:"
      elInpPwd <- inputElement $ def 
        & initialAttributes .~ ("class" =: inputStyle <> "type" =: "password")
        & inputElementConfig_setValue .~ evEmptyInput

      let eInput = domEvent Input elInpName
          dPwd = _inputElement_value elInpPwd
          dName = _inputElement_value elInpName 
      eDebounced <- debounce 0.8 eInput
      let eName = tagPromptlyDyn dName eDebounced
          dCredentials = Credentials <$> dName <*> dPwd
      
      ws <- webSocket "ws://localhost:8000/ws" $ def 
              & webSocketConfig_reconnect .~ False
              & webSocketConfig_send .~ ((:[]) <$> eName)
      let eWSMessage = DM.fromMaybe NoMessage <$> A.decode . BSL.fromStrict <$> _webSocket_recv ws
      dWSMessage <- holdDyn NoMessage eWSMessage 

      let eConnections = flip ffilter eWSMessage $ 
            \case 
              ConnectedClients _ -> True
              otherwise          -> False

          eUserResult = flip ffilter eWSMessage $ 
            \case
              UserExist name -> True
              NoUser         -> True
              otherwise      -> False
          
          eUserExistence = ffor eUserResult $ 
            \case 
              UserExist name -> "User `" <> name <> "` already exist. Connect to the chat with a password."
              NoUser         -> "No user found, Register as new one?"
              otherwise      -> "Error!"
      
          dNameLenght = ffor dName $ \name -> T.length name > 2
          
          dLoginConditions = fmap and $ sequence 
            [ dNameLenght
            , join $ ffor dWSMessage $ wsMessageEqualName dName
            ]
          
          dSignupConditions = fmap and $ sequence
            [ dNameLenght
            , not <$> (join $ ffor dWSMessage $ (wsMessageEqualName dName))
            ]
      
      let initialText = "Connect to chat with existing credentials or signup with new ones."
      dresult <- holdDyn initialText $ leftmost [eUserExistence, initialText <$ evEmptyInput]
      elClass "div" "flex items-center justify-center text-blue-500" $ dynText dresult

      evLoginRes <- sendButton dCredentials dLoginConditions "Connect" $ 
        FullRoute_Backend BackendRoute_Login :/ ()

      evSignupRes <- sendButton dCredentials dSignupConditions "Signup" $ 
        FullRoute_Backend BackendRoute_Signup :/ ()
      
      let evEitherResp = decodeResponse "Invalid username or password." evLoginRes
          evErr = either id (const "Login success.") <$> evEitherResp
          evOkUsr = fmapMaybe (either (const Nothing) id) evEitherResp 

          evEitherResp' = decodeResponse "401: Could not signup." evSignupRes 
          evMsx = either id 
                         (\case 
                             Nothing -> "Singup response msg not decoded."
                             Just beResp -> textOnly beResp
                         ) <$> evEitherResp'
          evEmptyInput = "" <$ evMsx
      dErr <- holdDyn "" $ leftmost [evErr, evMsx]
      elClass "div" "text-red-500" $ dynText $ dErr

      performEvent_ $ ffor evOkUsr $ \u -> do
        liftIO $ loggedTrigger appState (LoggedIn u)
      liftIO $ putStrLn "inside mainPage>>>>>>>>>"
      setRoute $ fforMaybe (updated (loggedAs appState)) $ \case
        Just (LoggedIn u) -> Just (FrontendRoute_User :/ _userName u)
        Just LoggedOut    -> Nothing
      -- ^ this is important to check that setRoute isn't fired without the
      --   loggedTrigger function being completed yet. It was abug toke me a
      --   while to figure out.

buttonStyleDisabled :: T.Text
buttonStyleDisabled = buttonStyle <> " disabled:bg-grey-200 disabled:opacity-50 disabled:border-grey-300"

wsMessageEqualName 
  :: (Reflex t, Functor (Dynamic t)) 
  => Dynamic t T.Text -> WSMessage -> Dynamic t Bool
wsMessageEqualName dName wsm = 
  case wsm of
    UserExist nameuser -> (nameuser ==) <$> dName
    otherwise          -> False <$ dName

enableDisable 
  :: Functor (Dynamic t)
  => Dynamic t Bool -> Dynamic t (Map T.Text T.Text)
enableDisable dConditions = ffor dConditions $ 
  \enabled ->
    if enabled
      then (fromList [("class", buttonStyle)])
      else (fromList [("class", buttonStyleDisabled), ("disabled","")])

sendButton 
  :: ( Monad m
     , DomBuilder t m
     , PostBuild t m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     )
  => Dynamic t Credentials 
  -> Dynamic t Bool 
  -> T.Text
  -> R (FullRoute BackendRoute FrontendRoute)
  -> m (Event t XhrResponse)
sendButton dCredentials enableCondition btnText route = do 
  (elBtn, _) <- elDynAttr' "button" (enableDisable enableCondition) $ text btnText
  let eClick = domEvent Click elBtn 
      eCredentials = tagPromptlyDyn dCredentials eClick

      url = getUrl route
      reqLogin = \credentials -> xhrRequest "POST" url $ def
        & xhrRequestConfig_withCredentials .~ True
        & xhrRequestConfig_headers .~ 
            ("Authorization" =: (ET.decodeUtf8 $ BSL.toStrict $ A.encode credentials))

  evRes <- performRequestAsync $ reqLogin <$> eCredentials
  return evRes
