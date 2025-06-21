{-# LANGUAGE OverloadedStrings #-}

module Common where

import Reflex.Dom.Core
import qualified Data.Text as T
import Obelisk.Route
import Data.Functor.Identity
import Common.Route 
-- import Obelisk.Route.Frontend
-- import Obelisk.Frontend

-- | Needs monad extended to @RoutedT@ because we run it in the @subRoute_@
-- used lift actually as a more generalised solution.
myButton :: DomBuilder t m => T.Text -> m (Element EventResult (DomBuilderSpace m) t, ())
myButton txt = 
  elAttr' "button" attr $ text txt
  where 
    attr = (  "class" =: "bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded" 
           <> "type" =: "button")

data AppState = AppState
  { loggedIn :: Bool
  , loggedUser :: Maybe T.Text
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
