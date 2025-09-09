{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

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
-- import Control.Monad.Trans (lift)
import Schema
import Common.Api (TweetUserResp(..))
import Common.MyFunctions (headSafe)
import Common

mainPage 
  :: ( ObeliskWidget t (R FrontendRoute) m)  
  => AppState t -> RoutedT t () m ()
mainPage appState = do

  buttonLogInOut appState $ FrontendRoute_Main :/ ()

  el "h2" $ text "Welcome to My X!"
  textArea <- textAreaElement $ def & initialAttributes .~ 
    ("placeholder" =: "Write your X here ..." 
    <> "class" =: "bg-blue-100 w-full p-2 rounded min-h-40")
  (elBtnPost, _) <- myButton "Post"
  
  let evPostClick = domEvent Click elBtnPost
      dTextArea = _textAreaElement_value textArea
      evPostMEUser = tagPromptlyDyn (loggedUser appState) $ evPostClick
      evMEUserLogged = ffilter isJust evPostMEUser
      evMEUserNotLog = ffilter (not . isJust) evPostMEUser
      url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Api_Submit
  
  prerender_ blank $ do 

    let mkTweet :: Maybe (Entity User) -> T.Text -> Maybe Tweet
        mkTweet Nothing areaText = Nothing
        mkTweet (Just (Entity k _)) areaText = Just $ Tweet 
          { tweetText = areaText
          , tweetReplyTo = Nothing
          , tweetOwner = k
          , tweetCreatedAt = Nothing
          }

        
        evMaybeTweet = flip tagPromptlyDyn evPostClick $
          mkTweet <$> loggedUser appState <*> dTextArea

        evTweet = fmapMaybe id evMaybeTweet


        evTweetReq = ffor evTweet $ \tweet -> XhrRequest
          { _xhrRequest_method = "POST"
          , _xhrRequest_url = url
          , _xhrRequest_config = def 
              & xhrRequestConfig_withCredentials .~ True
              & xhrRequestConfig_headers .~ ("Content-Type" =: "application/json")
              & xhrRequestConfig_sendData .~ BL.toStrict (A.encode tweet)
          }

    setRoute $ FrontendRoute_Login :/ () <$ evMEUserNotLog

    evResp <- performRequestAsync evTweetReq

    evPostBuild <- getPostBuild
    let evReload = leftmost [evPostBuild, () <$ evResp]
        urlPost = getUrl $ FullRoute_Backend BackendRoute_Api :/ Api_Posts
        xhrReq = XhrRequest
          { _xhrRequest_method = "GET"
          , _xhrRequest_url = urlPost
          , _xhrRequest_config = def & xhrRequestConfig_withCredentials .~ True
          }
    
    evResp' <- performRequestAsync $ xhrReq <$ evReload
    dRespT <- holdDyn "." $ (\r -> 
      fromMaybe ".." $ _xhrResponse_responseText r) <$> evResp'

    elClass "div" "text-gray-400 text-sm space-y-0" $ do 
      el "div" $ text "Click on tweet's username to see its profile." 
      el "div" $ text "Click on tweet's text to expand its reply if any."

    el "div" $ do
      let fromTtoTweets :: T.Text -> Maybe TweetUserResp
          fromTtoTweets = A.decode . BL.fromStrict . TE.encodeUtf8
          dMtweets = fromTtoTweets <$> dRespT
          dTUResp = fromMaybe (TweetUserResp [] []) <$> dMtweets
      dyn_ $ ffor dTUResp elTweetsList
      -- ^ dyn_ runs the Dynamic t (m ()), otherwise you'd only have a Dynamic not run.
      return ()
    return ()

  return ()


