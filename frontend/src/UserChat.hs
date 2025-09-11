{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-} 
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecursiveDo       #-}

module UserChat where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Reflex.Dom.Core
import Obelisk.Route
import Obelisk.Route.Frontend
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (void, join)
import Language.Javascript.JSaddle (MonadJSM)
import Control.Monad.Fix (MonadFix)
import Control.Applicative (liftA2)

import Common
import Common.Api
import Database.Schema
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

    let dIsPrivate = ffor dRouteUserName $ \name -> 
          not $ _userName user == name

        dButtonText = join $ ffor dIsPrivate $ \isPrivate ->
          if isPrivate
          then ffor dRouteUserName $ \name -> "Send Private to " <> name
          else constDyn "Send >"
        
        dButtonStyle = ffor dIsPrivate $ \isPrivate -> 
          if isPrivate
          then "class" =: buttonPrivateStyle
          else "class" =: buttonStyle

    dyn_ $ ffor (wsConn appState) $ \case 
      NoConnection        -> el "div" $ text "No connection."
      PublicConnection ws -> el "div" $ text "Loading..."
      AuthConnection ws   -> mdo 
        elClass "label" labelStyle $ text "Message:"
        
        elInpMess <- inputElement $ def 
          & initialAttributes .~ ("class" =: inputStyle)
          & inputElementConfig_setValue .~ evEmptyInput

        (elBtnSend, _) <- elDynAttr' "button" dButtonStyle $ dynText dButtonText

        let eSend = domEvent Click elBtnSend
            dMessText = _inputElement_value elInpMess
            dWSMess = join $ ffor dIsPrivate $ \isPrivate -> 
              if isPrivate 
              then NewPrivate <$> (mkPrivateFEMessage user <$> dRouteUserName <*> dMessText)
              else NewMessage <$> (mkPublicFEMessage user <$> dMessText)
            -- ^ Event does not have Applicative, but Dynamic does.
            eWSMess = tagPromptlyDyn dWSMess eSend
            evEmptyInput = "" <$ eWSMess
            
        performEvent_ $ (liftIO . wsSendTrigger appState . A.encode) <$> eWSMess
        let evmWSMessage = A.decode . BSL.fromStrict <$> _webSocket_recv ws
        dChatMessages <- foldDyn (:) [] $ feMessage <$> evmWSMessage 
        elClass "div" "flex flex-col gap-0 p-4" $ void $ do
          simpleList dChatMessages $ \dMsg -> 
            elClass "div" "flex items-start p-0 " $ dynText dMsg


mkPublicFEMessage :: User -> T.Text  -> FEMessage
mkPublicFEMessage user mess = FEMessage
  { femId        = Nothing
  , femBody      = mess
  , femTimestamp = Nothing
  , femOwner     = user
  , femPrivate   = Nothing
  }

mkPrivateFEMessage :: User ->  Username -> T.Text -> FEMessage
mkPrivateFEMessage user recipient mess = FEMessage
  { femId        = Nothing
  , femBody      = mess
  , femTimestamp = Nothing
  , femOwner     = user
  , femPrivate   = Just [recipient]
  }

-- | translates websocket messages into Frontend messages, 
--   @Text@s ready for the chat.
feMessage :: Maybe WSMessage -> T.Text
feMessage wsm = case wsm of
  Nothing -> "*** Non Valid Message ***"
  Just m  -> case m of
    NewMessage m       -> (_userName $ femOwner m) <> "> " <> femBody m
    NewPrivate (FEMessage _ body _ owner (Just (x:xs))) ->
      (_userName owner) <> "(@" <> x <> ")> " <> body
    ConnectedClients _ -> "Client connection event."
    otherwise          -> "TODO: unknown message."

