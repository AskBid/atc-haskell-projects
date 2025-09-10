{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

module TweetPage where

import Common.Route 
import Reflex.Dom.Core
import Common
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Frontend
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Database.Persist.Sql
import Data.Maybe

import Schema
import Common.Api (TweetsOwnersResp(..))
import Common

tweetPage 
  :: ObeliskWidget t (R FrontendRoute) m
  => AppState t -> T.Text -> RoutedT t a m ()
tweetPage appState tweetId = do

  prerender_ blank $ do 

    evPostBuild <- getPostBuild

    requestAndListTweets evPostBuild $ 
      FullRoute_Backend 
      BackendRoute_Api :/ Api_PostReplies tweetId

    el "h2" $ text $ "Reply to tweet (id: " <> tweetId <> ")"
    textArea <- textAreaElement $ def & initialAttributes .~ 
      (  "placeholder" =: "Write reply here ..." 
      <> "class"       =: "bg-blue-100 w-full p-2 rounded min-h-40")
    (elBtnPost, _) <- myButton "Reply"
    return ()
  return ()


