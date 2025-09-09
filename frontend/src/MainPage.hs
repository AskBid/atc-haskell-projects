{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase           #-}

module MainPage where

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
import Data.Time (getCurrentTime, UTCTime(..), fromGregorian, secondsToDiffTime)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Schema
import Common.Api (TweetUserResp(..))
import Common.MyFunctions (headSafe)
import Common

mainPage 
  :: ObeliskWidget t (R FrontendRoute) m
  => AppState t -> RoutedT t () m ()
mainPage appState = do

  prerender_ blank $ do 

    buttonLogInOut appState $ FrontendRoute_Main :/ ()

    el "h2" $ text "Welcome to My Tweetter!"
    textArea <- textAreaElement $ def & initialAttributes .~ 
      ("placeholder" =: "Write your Tweet here ..." 
      <> "class" =: "bg-blue-100 w-full p-2 rounded min-h-40")
    (elBtnPost, _) <- myButton "Post"
  
    let evPostClick = domEvent Click elBtnPost
        dTextArea = _textAreaElement_value textArea

        evPostWithMEUser = tagPromptlyDyn (loggedUser appState) $ evPostClick
        evMEUserLogged = ffilter isJust evPostWithMEUser
        evMEUserNotLog = ffilter (not . isJust) evPostWithMEUser

    setRoute $ FrontendRoute_Login :/ () <$ evMEUserNotLog
    
    ----------------
    -- posting tweet
    let evMkTweet = fmapMaybe id $ 
          tagPromptlyDyn 
            (mkTweet <$> loggedUser appState <*> dTextArea)
            evPostClick

        xhrSubmitPost = \tweet -> XhrRequest
          { _xhrRequest_method = "POST"
          , _xhrRequest_url = getUrl $ 
              FullRoute_Backend 
              BackendRoute_Api :/ 
              Api_SubmitPost
          , _xhrRequest_config = def 
              & xhrRequestConfig_withCredentials .~ True
              & xhrRequestConfig_headers .~ ("Content-Type" =: "application/json")
              & xhrRequestConfig_sendData .~ BL.toStrict (A.encode tweet)
          }

    evTweetPostResp <- performRequestAsync $ xhrSubmitPost <$> evMkTweet
    -- posting tweet
    ----------------

    elClass "div" "text-gray-400 text-sm space-y-0" $ do 
      el "div" $ text "Click on tweet's username to see its profile." 
      el "div" $ text "Click on tweet's text to expand its reply if any."

    el "div" $ do

      evPostBuild <- getPostBuild

      let evReload = leftmost [evPostBuild, () <$ evTweetPostResp]
          -- ^ we refresh general posts feed at page start or tweet post.
          xhrGetPosts = XhrRequest
            { _xhrRequest_method = "GET"
            , _xhrRequest_url = getUrl $ 
                FullRoute_Backend 
                BackendRoute_Api :/ 
                Api_Posts
            , _xhrRequest_config = def & xhrRequestConfig_withCredentials .~ True
            }
    
      evGetPostsResp <- performRequestAsync $ xhrGetPosts <$ evReload
      
      dPosts <- holdDyn Nothing $ ffor evGetPostsResp $ \resp ->
        case _xhrResponse_responseText resp of
          Nothing   -> Nothing
          Just text -> A.decode . BL.fromStrict . TE.encodeUtf8 $ text

      dyn_ $ ffor dPosts $ \case
        Nothing     -> el "div" $ text "something went wrong."
        Just tuResp -> elTweetsList tuResp

      return ()
    return ()
  return ()

mkTweet :: Maybe (Entity User) -> T.Text -> Maybe Tweet
mkTweet Nothing _                    = Nothing
mkTweet (Just (Entity k _)) areaText = Just $ Tweet 
  { tweetText = areaText
  , tweetReplyTo = Nothing
  , tweetOwner = k
  , tweetCreatedAt = Nothing
  }

