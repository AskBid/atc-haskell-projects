{-# LANGUAGE OverloadedStrings #-}

module Common where

import Reflex.Dom.Core
import qualified Data.Text as T

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
