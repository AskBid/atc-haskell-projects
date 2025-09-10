{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE LambdaCase          #-}

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

  prerender_ blank $ mdo 

    evPostBuild <- getPostBuild

    let evmTORupdated = updated dmTOR
    evmTORfirst <- headE evmTORupdated
    dmTORfirst <- holdDyn Nothing evmTORfirst

    dyn_ $ ffor dmTORfirst $ \case 
      Nothing -> el "div" $ text "Loading"
      Just (TweetsOwnersResp _ users mParentTweet) -> 
        case mParentTweet of
          Nothing -> el "div" $ text "err: No tweet retrieved."
          Just parentTweet -> elTweet users parentTweet

    el "h2" $ text $ "Reply to tweet (id: " <> tweetId <> ")"
    textArea <- textAreaElement $ def & initialAttributes .~ 
      (  "placeholder" =: "Write reply here ..." 
      <> "class"       =: "bg-blue-100 w-full p-2 rounded min-h-40")
    (elBtnPost, _) <- myButton "Reply"

    dmTOR <- requestAndListTweets evPostBuild $ 
      FullRoute_Backend 
      BackendRoute_Api :/ Api_PostReplies tweetId

    return ()
  return ()


