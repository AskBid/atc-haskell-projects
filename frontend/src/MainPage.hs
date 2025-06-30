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
import Data.Functor.Identity
import Common.Api (LoginReq(..))
-- import Control.Monad.Trans (lift)


mainPage 
  :: ( ObeliskWidget t (R FrontendRoute) m)  
  => AppState t -> RoutedT t () m ()
mainPage appState = do
  -- dCurrentRoute <- askRoute
  dyn_ $ (buttonLogInOut $ FrontendRoute_Signup :/ ()) <$> (loggedIn appState)
  el "h2" $ text "Welcome to My X!"
  area <- textAreaElement $ def & initialAttributes .~ 
    ("placeholder" =: "Write your X here ..." <> "class" =: "bg-blue-100 w-full p-2 rounded min-h-40")
  return ()


buttonLogInOut 
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , Prerender t m
     )
  => (R FrontendRoute) -> Bool -> RoutedT t () m ()
buttonLogInOut route logBool = 
  if not logBool 
  then do 
    (btnInEl, _) <- myButton "Login"
    let loginClick = domEvent Click btnInEl
    setRoute $ FrontendRoute_Login :/ () <$ loginClick
  else do
    (btnOutEl, _) <- myButton "Logout"
    let logoutClick = domEvent Click btnOutEl
    let url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Tail_Logout
    let xhrReq = XhrRequest { 
        _xhrRequest_method = "GET"
      , _xhrRequest_url = url
      , _xhrRequest_config = def & xhrRequestConfig_withCredentials .~ True
      }
      -- ^ By default, browsers do not send cookies or store cookies from cross-origin 
      --   requests made via fetch/XHR unless explicitly told to.
      --   with @xhrRequestConfig_withCredentials .~ True@ you're telling the browser
      --   to include my cookies in this request, and also accept any Set-Cookie headers 
      --   in the response
    dResp <- prerender (pure never) $ do  
      evResp <- performRequestAsync $ xhrReq <$ logoutClick
      return evResp
    setRoute $ route <$ logoutClick
