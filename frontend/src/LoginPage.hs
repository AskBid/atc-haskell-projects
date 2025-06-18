{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

module LoginPage where

import Common.Route 
import Reflex.Dom.Core
import Reflex.Dom
import Common
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Frontend
import qualified Data.Text as T
import Data.Functor.Identity
import Common.Api (LoginReq(..))
import Control.Monad.Trans (lift)


loginPage :: ObeliskWidget t (R FrontendRoute) m  => RoutedT t () m ()
loginPage = do
  el "h2" $ text "Login here."
  el "label" $ text "Username: "
  usrEl <- inputElement $ def & initialAttributes .~ 
    ("class" =: "border")
  el "label" $ text "Password: "
  pwdEl <- inputElement $ def & initialAttributes .~ 
    ("class" =: "border" <> "type" =: "password")
  (btnEl, _) <- lift $ myButton "Submit"
  let submitClick = domEvent Click btnEl
  let usrDyn = _inputElement_value usrEl
  let pwdDyn = _inputElement_value pwdEl
  let logReqDyn = LoginReq <$> usrDyn <*> pwdDyn
  prerender (pure ()) $ do 
    let loginReqEv = tagPromptlyDyn logReqDyn submitClick
    -- ^ loginReqEv, (postJson "text"), submitClick
    --         ==
    -- e l,        (l -> xhr),        e c 
    -- :: (l -> xhr) -> e l -> e xhr 
    -- (postJson "text") <$> e l :: e xhr 
    -- performRequestAsync       :: e xhr -> m (e xhr) 
    let url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Tail_Login
    respEv <- (performRequestAsync $ (postJson url) <$> loginReqEv)
    let maybetextResp = _xhrResponse_responseText <$> respEv
    let resp = maybe "error" id <$> maybetextResp
    dynResp <- holdDyn "No Response received yet" resp
    -- el "br" blank
    -- el "br" blank
    -- el "h1" $ text "----->>>  repsonse: "
    -- el "h3" $ dynText $ (\xhrRB -> case xhrRB of
    --   XhrResponseBody_Default _     -> "XhrResponseBody_Default"
    --   XhrResponseBody_Text text     -> text
    --   XhrResponseBody_Blob _        -> "XhrResponseBody_Blob"
    --   XhrResponseBody_ArrayBuffer _ -> "XhrResponseBody_ArrayBuffer"
    --   otherwise                     -> "others") <$> dynResp
    -- let maybeText = _xhrResponse_responseText <$> respEv
    -- let resp2 = (maybe "error" id <$> maybeText)
    -- dynResp2 <- holdDyn "::" resp2
    -- el "h3" $ dynText dynResp2
    el "h3" $ dynText dynResp
    return ()
  return ()

-- | @fullRouteEncoder@ has an `Either Text` as a first (check) argument
--   to make it Identity as required from the use of @encode@, we need first to pass
--   it under the check of @checkEncoder@ which gets rid off the uncertainty if we receive text
--   or not.
safeEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
safeEncoder = 
  case checkEncoder fullRouteEncoder of
    Left x    -> undefined
    Right enc -> enc

-- | Given a FullRoute returns a Text Url
--  getUrl (FullRoute_Frontend (ObeliskRoute_App FrontendRoute_Login) :/ ())
--  getUrl $ FullRoute_Backend BackendRoute_Api :/ Tail_Login
getUrl :: R (FullRoute BackendRoute FrontendRoute) -> T.Text
getUrl route = T.intercalate "/" $ fst pageName
  where 
    pageName = encode safeEncoder route
