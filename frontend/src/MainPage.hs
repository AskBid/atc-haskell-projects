{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

module MainPage where

import Common.Route 
import Reflex.Dom.Core
-- import Reflex.Dom
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
import Data.Time (getCurrentTime, UTCTime)
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
  
  let dTextArea = _textAreaElement_value textArea
      evPostMEUser = tagPromptlyDyn (loggedUser appState) $ domEvent Click elBtnPost
      evMEUserLogged = ffilter isJust evPostMEUser
      evMEUserNotLog = ffilter (not . isJust) evPostMEUser
      url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Api_Submit
      -- evTextTweet = tagPromptlyDyn dTextArea evPostLogged
  
  --                               Event t (IO UTCTime)
  dResp <- prerender (pure never) $ do 
    let evIoUTCTime = getCurrentTime <$ evMEUserLogged
    evTime <- performEvent $ liftIO <$> evIoUTCTime

    let mkTweet :: Maybe (Entity User) -> T.Text -> UTCTime -> Maybe Tweet
        mkTweet Nothing areaText time = Nothing
        mkTweet (Just (Entity k _)) areaText time = Just $ Tweet 
          { tweetText = areaText
          , tweetReplyTo = Nothing
          , tweetOwner = k
          , tweetCreatedAt = time
          }

        -- dMetaMkTweet :: Dynamic t (T.Text -> UTCTime -> Maybe Tweet)
        dMetaMkTweet = mkTweet <$> (loggedUser appState)
        -- \/ :: Event t (T.Text -> UTCTime -> Maybe Tweet)
        evMetaMkTweet = tagPromptlyDyn dMetaMkTweet evMEUserLogged
        -- \/ :: Event t (Event t Tweet)
        evTweet = coincidence $ ffor evMetaMkTweet $ 
          \metaMkTweet -> attachPromptlyDynWithMaybe metaMkTweet dTextArea evTime
        -- ^ (T.Text -> UTCTime -> Maybe Tweet) -> Event t Tweet
        --   attachPromptlyDynWith :: (a -> b -> c) -> Dynamic t a -> Event t b -> Event t c
        --   with/maybe :: (a -> b -> Maybe c) -> Dynamic t a -> Event t b -> Event t c
        --   with Maybe it filters out the firing if `c` is Nothing.

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
    return evResp

  prerender_ (el "h1" $ text "Loading...") $ do
    evPostBuild <- getPostBuild
    let evReload = leftmost [evPostBuild, () <$ switchDyn dResp]
    let url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Api_Posts
        xhrReq = XhrRequest
          { _xhrRequest_method = "GET"
          , _xhrRequest_url = url
          , _xhrRequest_config = def & xhrRequestConfig_withCredentials .~ True
          }
    
    evResp <- performRequestAsync $ xhrReq <$ evReload
    dRespT <- holdDyn "." $ (\r -> fromMaybe ".." $ _xhrResponse_responseText r) <$> evResp
    
    el "div" $ do
      let fromTtoTweets :: T.Text -> Maybe TweetUserResp
          fromTtoTweets = A.decode . BL.fromStrict . TE.encodeUtf8
          dMtweets = fromTtoTweets <$> dRespT
          dTUResp = fromMaybe (TweetUserResp [] []) <$> dMtweets
      dyn_ $ ffor dTUResp $ \tuResp -> mapM_ (elTweet $ users tuResp) (tweets tuResp)     
      -- ^ dyn_ runs the Dynamic t (m ()), otherwise you'd only have a Dynamic not run.
      return ()
    return ()

  return ()


