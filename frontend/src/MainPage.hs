{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

module MainPage where

import Common.Route 
import Reflex.Dom.Core
-- import Reflex.Dom
import Common
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Frontend
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity
import Common.Api (LoginReq(..))
-- import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Schema
import Data.Maybe
import Database.Persist.Sql

mainPage 
  :: ( ObeliskWidget t (R FrontendRoute) m)  
  => AppState t -> RoutedT t () m ()
mainPage appState = do
  buttonLogInOut appState $ FrontendRoute_Main :/ ()
  -- el "h2" $ dynText (T.pack . show <$> loggedIn appState)
  el "h2" $ text "Welcome to My X!"
  area <- textAreaElement $ def & initialAttributes .~ 
    ("placeholder" =: "Write your X here ..." <> "class" =: "bg-blue-100 w-full p-2 rounded min-h-40")
   
  prerender (pure ()) $ do
    evPostBuild <- getPostBuild
    let url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Api_Posts
        xhrReq = XhrRequest
          { _xhrRequest_method = "GET"
          , _xhrRequest_url = url
          , _xhrRequest_config = def & xhrRequestConfig_withCredentials .~ True
          }
    evResp <- performRequestAsync $ xhrReq <$ evPostBuild
    dRespT <- holdDyn "." $ (\r -> fromMaybe ".." $ _xhrResponse_responseText r) <$> evResp
    let fromTtoTweets :: T.Text -> Maybe [Tweet]
        fromTtoTweets = A.decode . BL.fromStrict . TE.encodeUtf8
        dMtweets = fromTtoTweets <$> dRespT
    el "h1" $ dynText $ 
      (\twts -> tweetText $ head twts) <$> 
      (\mtwts -> fromMaybe ([Tweet "" (Just $ toSqlKey 1) (toSqlKey 1)]) mtwts) <$> dMtweets
    el "h1" $ dynText dRespT
    return ()

  return ()


buttonLogInOut
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , Prerender t m
     , PostBuild t m
     )
  => AppState t -> R FrontendRoute -> RoutedT t () m ()
buttonLogInOut appState route = do
  let dynLoggedIn = loggedIn appState  -- Dynamic t Bool

  dyn_ $ ffor dynLoggedIn $ \logBool ->
    if not logBool
    then do
      (btnInEl, _) <- myButton "Login"
      let loginClick = domEvent Click btnInEl
      setRoute $ FrontendRoute_Login :/ () <$ loginClick
    else do
      (btnOutEl, _) <- myButton "Logout"
      let logoutClick = domEvent Click btnOutEl
          url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Api_Logout
          xhrReq = XhrRequest
            { _xhrRequest_method = "GET"
            , _xhrRequest_url = url
            , _xhrRequest_config = def & xhrRequestConfig_withCredentials .~ True
            }
      -- ^ By default, browsers do not send cookies or store cookies from cross-origin 
      --   requests made via fetch/XHR unless explicitly told to.
      --   with @xhrRequestConfig_withCredentials .~ True@ you're telling the browser
      --   to include my cookies in this request, and also accept any Set-Cookie headers 
      --   in the response
      dynEvLogoutResp <- prerender (pure never) $ do  
        evResp <- performRequestAsync $ xhrReq <$ logoutClick
        let evLogoutSuccess = ffilter (statusCheck 200 300) evResp
        -- let evLoginTriggerIO = (\_ -> loginTrigger appState False) <$> evLogoutSuccess
        performEvent_ $ ffor evLogoutSuccess $ \_ -> liftIO $ do
          putStrLn "Logout successful, triggering login state False"
          loginTrigger appState False
        -- performEvent_ $ liftIO <$> evLoginTriggerIO
        return evLogoutSuccess
      -- ^ Dynamic t (Event t XhrResponse) << returned from `prerender`
      setRoute $ route <$ switchDyn dynEvLogoutResp

