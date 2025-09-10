{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

module UserPage where

import Common.Route 
import Reflex.Dom.Core
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Frontend
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Database.Persist.Sql
import Data.Maybe
import Data.Time (getCurrentTime, UTCTime(..), fromGregorian, secondsToDiffTime)
import Control.Monad.IO.Class (liftIO, MonadIO)
-- import Control.Monad.Trans (lift)
import Schema
import Common.Api (TweetsOwnersResp(..))
import Common.MyFunctions (headSafe)
import FrontendCommon.Common
import FrontendCommon.TweetRender

userPage 
  :: (ObeliskWidget t (R FrontendRoute) m)
  => AppState t -> T.Text -> RoutedT t a m ()
userPage appState username = do

  prerender_ blank $ do 

    el "h1" $ text $ "Profile for " <> username
    elClass "h3" "text-gray-400" $ text "TODO: user's attributes ..."
    el "h3" $ text $ username <> "`s tweets:" 
    elClass "div" "text-gray-400" $ 
      text "TODO: handle replies distinction from main posts."
    -------------
    -- list posts
    evPostBuild <- getPostBuild

    requestAndListTweets evPostBuild $ 
      FullRoute_Backend
      BackendRoute_Api :/ Api_PostsByUser username

    -- list posts
    -------------
    return ()

  return ()


