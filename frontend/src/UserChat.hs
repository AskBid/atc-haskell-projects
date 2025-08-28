{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-} 
{-# LANGUAGE LambdaCase        #-}

module UserChat where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Reflex.Dom.Core
import Obelisk.Route
import Obelisk.Route.Frontend
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (void)
import Language.Javascript.JSaddle (MonadJSM)
import Control.Monad.Fix (MonadFix)

import Common
import Common.Api
import Schema
import Common.Route

userChat 
  :: ( Prerender t m
     , Monad m
     , Routed t T.Text m
     , Routed t T.Text (Client m)
     , SetRoute t (R FrontendRoute) (Client m)
     ) 
  => AppState t -> m ()
userChat appState = do

  prerender_ blank $ do  
    dName <- askRoute 
    el "h1" $ dynText dName
    dyn_ $ ffor (loggedAs appState) $ \case
      LoggedIn u  -> chatPanel dName
      LoggedOut   -> redirectToAuth 
      -- ^ TODO could send attempted name with parameter to set as value
      --   in the login input element.
    return ()

redirectToAuth 
  :: ( SetRoute t (R FrontendRoute) (Client m)
     , Monad m
     , PostBuild t m 
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
redirectToAuth = do 
  ePostbuild <- getPostBuild
  setRoute $ FrontendRoute_Main :/ () <$ ePostbuild

chatPanel 
  :: ( Monad m
     , MonadIO m
     , PostBuild t m 
     , DomSpace (DomBuilderSpace m)
     , TriggerEvent t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , MonadJSM m
     , MonadHold t m
     , MonadFix m
     , Adjustable t m
     , DomBuilder t m
     ) 
  => Dynamic t T.Text -> m ()
chatPanel dName = do
  -- user == userRoute -> public chat
  -- user /= userRoute -> private chat
  elClass "div" divVerticalStyle $ do

    ePostBuild <- getPostBuild
    let eName = tagPromptlyDyn dName ePostBuild
        eUrl = ("ws://localhost:8000/ws/user/" <>) <$> eName

    let eSocket = ffor eUrl $ \url -> do 
          elClass "label" labelStyle $ text "Message:"
          elInpMess <- inputElement $ def & initialAttributes .~ ("class" =: inputStyle)
          (elBtnSend, _) <- elClass' "button" buttonStyle $ text "Send >"

          let eSend = domEvent Click elBtnSend
              dMessText = _inputElement_value elInpMess
              eMessText = tagPromptlyDyn dMessText eSend
              eWSMess = NewMessage <$> mkMessage <$> eMessText 
          
          let wsConfig = def & webSocketConfig_reconnect .~ False
                             & webSocketConfig_send .~ ((:[]) <$> A.encode <$> eWSMess)

          liftIO $ putStrLn $ "Opening WebSocket at: " ++ T.unpack url
          ws <- webSocket url wsConfig
          -- ^ Event keep on triggering when new message comes from WS backend

          let evmWSMessage = A.decode . BSL.fromStrict <$> _webSocket_recv ws

          performEvent_ $ liftIO . putStrLn . T.unpack . feMessage <$> evmWSMessage

          dChatMessages <- foldDyn (:) [] $ feMessage <$> evmWSMessage 

          elClass "div" "flex flex-col gap-0 p-4" $ void $ do
            simpleList dChatMessages $ \dMsg -> elClass "div" "p-0 " $ dynText dMsg

    void $ widgetHold (el "div" $ text "No connection.") eSocket 

mkMessage :: T.Text -> Message
mkMessage t = Message 
  { _messageTimestamp = Nothing
  , _messageBody = t
  -- , messageOwner = Nothing
  -- , messagePrivate = Nothing
  }


