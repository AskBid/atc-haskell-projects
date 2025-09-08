{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

module UserPage where

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
import Data.Time (getCurrentTime, UTCTime(..), fromGregorian, secondsToDiffTime)
import Control.Monad.IO.Class (liftIO, MonadIO)
-- import Control.Monad.Trans (lift)
import Schema
import Common.Api (TweetUserResp(..))
import Common.MyFunctions (headSafe)
import Common

userPage 
  :: ( ObeliskWidget t (R FrontendRoute) m)  
  => AppState t -> T.Text -> RoutedT t a m ()
userPage appState username = do
  prerender_ blank $ do 
    el "h1" $ text $ "Profile for " <> username
    elClass "h3" "text-gray-400" $ text "TODO: user's attributes ..."
    el "h3" $ text $ username <> "`s tweets:"
    evPostBuild <- getPostBuild
    let urlPost = getUrl $ FullRoute_Backend BackendRoute_Api :/ Api_PostsByUser username 
        xhrReq = XhrRequest
          { _xhrRequest_method = "GET"
          , _xhrRequest_url = urlPost
          , _xhrRequest_config = def & xhrRequestConfig_withCredentials .~ True
          }
    
    evResp' <- performRequestAsync $ xhrReq <$ evPostBuild
    dRespT <- holdDyn "." $ (\r -> 
      fromMaybe ".." $ _xhrResponse_responseText r) <$> evResp'
    
    el "div" $ do
      let fromTtoTweets :: T.Text -> Maybe TweetUserResp
          fromTtoTweets = A.decode . BL.fromStrict . TE.encodeUtf8
          dMtweets = fromTtoTweets <$> dRespT
          dTwUsrResp = fromMaybe (TweetUserResp [] []) <$> dMtweets
      dyn_ $ ffor dTwUsrResp $ \twUsrResp -> mapM_ (elTweet $ users twUsrResp) (tweets twUsrResp)     
      -- ^ dyn_ runs the Dynamic t (m ()), otherwise you'd only have a Dynamic not run.
      return ()
    return ()

  return ()


