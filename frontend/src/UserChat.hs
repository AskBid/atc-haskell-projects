{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-} 

module UserChat where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Reflex.Dom.Core
import Obelisk.Route
import Obelisk.Route.Frontend
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

import Common
import Common.Api
import Schema
import Common.Route

userChat 
  :: ( Prerender t m
     , Monad m
     , Routed t T.Text m
     , Routed t T.Text (Client m)
     ) 
  => m ()
userChat = do
  prerender_ blank $ do  
    dName <- askRoute
    el "h1" $ dynText dName
    elClass "div" divVerticalStyle $ do

      ePostBuild <- getPostBuild
      let eName = tagPromptlyDyn dName ePostBuild
          eUrl = ("ws://localhost:8000/ws/" <>) <$> eName

      let eSocket = ffor eUrl $ \url -> do 
            elClass "label" labelStyle $ text "Message:"
            elInpMess <- inputElement $ def & initialAttributes .~ ("class" =: inputStyle)
            (elBtnSend, _) <- elClass' "button" buttonStyle $ text "Send >"

            let eSend = domEvent Click elBtnSend
                dMessText = _inputElement_value elInpMess
                eMessText = tagPromptlyDyn dMessText eSend
                eWSMess = NewMessage <$> mkMessage <$> eMessText 
            
            let wsConfig = def & webSocketConfig_reconnect .~ True
                               & webSocketConfig_send .~ ((:[]) <$> A.encode <$> eWSMess)
            liftIO $ putStrLn $ "Opening WebSocket at: " ++ T.unpack url
            ws <- webSocket url wsConfig
            -- ^ Event keep on triggering when new message comes from WS backend

            let evmWSMessage = A.decode . BSL.fromStrict <$> _webSocket_recv ws

            performEvent_ $ liftIO . putStrLn . T.unpack . feMessage <$> evmWSMessage

            dChatMessages <- foldDyn (:) [] $ feMessage <$> evmWSMessage 

            elClass "div" divVerticalStyle $ void $ do
              simpleList dChatMessages $ \dMsg -> el "div" $ dynText dMsg

      widgetHold (el "div" $ text "No connection.") eSocket 

    return ()

mkMessage :: T.Text -> Message
mkMessage t = Message 
  { messageTimestamp = Nothing
  , messageText = t
  , messageOwner = Nothing
  , messagePrivate = Nothing
  }
