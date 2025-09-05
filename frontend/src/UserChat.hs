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
import Control.Applicative (liftA2)

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
    liftIO $ putStrLn "FE: inside userChat"
    performEvent_ $ ffor (updated (loggedAs appState)) $
      \name -> liftIO $ putStrLn $ show name
    dyn_ $ ffor (loggedAs appState) $ \case
      Loading    -> el "div" $ text "loading..."
      LoggedIn u -> chatPanel dName u appState
      LoggedOut  -> do 
        liftIO $ putStrLn "FE: LoggedOut"
        redirectToAuth 
      -- ^ TODO could send attempted name with parameter to set as 
      --   value in the login input element.
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
  => Dynamic t T.Text -> User -> AppState t -> m ()
chatPanel dRouteUserName user appState = do
  elClass "div" divVerticalStyle $ do
    -- ePostBuild <- getPostBuild
    -- let eUrl = ("ws://localhost:8000/ws/user/" <> (_userName user)) <$ ePostBuild
    let dButtonText = ffor dRouteUserName $ \name ->  
          if _userName user == name 
          then "Send >"
          else "Send Private to " <> name
        dButtonStyle = ffor dRouteUserName $ \name ->  
          if _userName user == name 
          then "class" =: buttonStyle
          else "class" =: buttonPrivateStyle
        eWsConn = updated (wsConn appState)

    dyn_ $ ffor (wsConn appState) $ \case 
      NoConnection        -> el "div" $ text "No connection."
      PublicConnection ws -> el "div" $ text "Loading..."
      AuthConnection ws   -> do 
        elClass "label" labelStyle $ text "Message:"
        elInpMess <- inputElement $ def & initialAttributes .~ ("class" =: inputStyle)
        (elBtnSend, _) <- elDynAttr' "button" dButtonStyle $ dynText dButtonText
        let eSend = domEvent Click elBtnSend
            dMessText = _inputElement_value elInpMess
            dWSMess = NewMessage <$> (mkFEMessage <$> dMessText)
            -- ^ Event does not have Applicative, but Dynamic does.
            eWSMess = tagPromptlyDyn dWSMess eSend
            -- dPrivate = (Just . (:[])) <$> dRouteUserName
            -- dWSMess = NewMessage <$> (mkFEMessage <$> dMessText <*> dPrivate)
        performEvent_ $ (liftIO . wsSendTrigger appState . A.encode) <$> eWSMess
        let evmWSMessage = A.decode . BSL.fromStrict <$> _webSocket_recv ws
        dChatMessages <- foldDyn (:) [] $ feMessage <$> evmWSMessage 
        elClass "div" "flex flex-col gap-0 p-4" $ void $ do
          simpleList dChatMessages $ \dMsg -> elClass "div" "p-0 " $ dynText dMsg

mkFEMessage :: T.Text -> FEMessage
mkFEMessage mess = FEMessage 
  { femId        = Nothing
  , femBody      = mess
  , femTimestamp = Nothing
  , femOwner     = "bob"
  , femPrivate   = Nothing -- pRecipient
  }


