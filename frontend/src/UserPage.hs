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

import FrontendCommon.TweetRender

userPage 
  :: (ObeliskWidget t (R FrontendRoute) m)
  => T.Text -> RoutedT t a m ()
userPage username = do

  prerender_ blank $ do 

    el "h1" $ text $ "Profile for " <> username
    elClass "h3" "text-gray-400" $ text "TODO: user's attributes ..."
    el "h3" $ text $ username <> "`s tweets:" 
    elClass "div" "text-gray-400" $ 
      text "TODO: handle replies distinction from main posts."
    -------------
    -- list posts
    evPostBuild <- getPostBuild

    _ <- requestAndListTweets evPostBuild $ 
      FullRoute_Backend
      BackendRoute_Api :/ Api_PostsByUser username

    -- list posts
    -------------
    return ()

  return ()


