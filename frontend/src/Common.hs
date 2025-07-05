{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Common where

import Reflex.Dom.Core
import qualified Data.Text as T
import Obelisk.Route
import Data.Functor.Identity
import Common.Route 
import Obelisk.Route.Frontend
import Control.Monad.IO.Class (liftIO)
-- import Obelisk.Frontend

-- | Needs monad extended to @RoutedT@ because we run it in the @subRoute_@
-- used lift actually as a more generalised solution.
myButton :: DomBuilder t m => T.Text -> m (Element EventResult (DomBuilderSpace m) t, ())
myButton txt = 
  elAttr' "button" attr $ text txt
  where 
    attr = (  "class" =: "bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded" 
           <> "type" =: "button")

data AppState t = AppState 
  { loggedIn :: Dynamic t Bool
  , loggedUser :: Maybe T.Text
  , loginTrigger :: Bool -> IO ()
  }

-- | @fullRouteEncoder@ has an `Either Text` as a first (check) argument
--   to make it Identity as required from the use of @encode@, we need first to pass
--   it under the check of @checkEncoder@ which gets rid off the uncertainty if we receive text
--   or not.
safeEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
safeEncoder = 
  case checkEncoder fullRouteEncoder of
    Left err  -> error $ "Encoder check failed in safeEncoder: " <> T.unpack err
    Right enc -> enc

-- | Given a FullRoute returns a Text Url
--  getUrl (FullRoute_Frontend (ObeliskRoute_App FrontendRoute_Login) :/ ())
--  getUrl $ FullRoute_Backend BackendRoute_Api :/ Tail_Login
getUrl :: R (FullRoute BackendRoute FrontendRoute) -> T.Text
getUrl route = T.intercalate "/" $ fst pageName
  where 
    pageName = encode safeEncoder route

statusCheck :: Word -> Word -> XhrResponse -> Bool
statusCheck min max xhr
  | status >= min && status < max = True
  | otherwise                     = False
  where status = _xhrResponse_status xhr

-- | widget that shows login button if not loggedin and logout button otherwise
--   takes appState to derive loggedin state and a route to decide where the logout
--   button redirects.
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
