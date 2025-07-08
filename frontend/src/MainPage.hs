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
-- import Control.Monad.Trans (lift)
-- import Control.Monad.IO.Class (liftIO)
import Schema
import Common.Api (TweetUserResp(..))

mainPage 
  :: ( ObeliskWidget t (R FrontendRoute) m)  
  => AppState t -> RoutedT t () m ()
mainPage appState = do
  buttonLogInOut appState $ FrontendRoute_Main :/ ()
  -- el "h2" $ dynText (T.pack . show <$> loggedIn appState)
  el "h2" $ text "Welcome to My X!"
  _ <- textAreaElement $ def & initialAttributes .~ 
    ("placeholder" =: "Write your X here ..." <> "class" =: "bg-blue-100 w-full p-2 rounded min-h-40")
   
  prerender (el "h1" $ text "Loading...") $ do
    evPostBuild <- getPostBuild
    let url = getUrl $ FullRoute_Backend BackendRoute_Api :/ Api_Posts
        xhrReq = XhrRequest
          { _xhrRequest_method = "GET"
          , _xhrRequest_url = url
          , _xhrRequest_config = def & xhrRequestConfig_withCredentials .~ True
          }
    evResp <- performRequestAsync $ xhrReq <$ evPostBuild
    dRespT <- holdDyn "." $ (\r -> fromMaybe ".." $ _xhrResponse_responseText r) <$> evResp
    el "div" $ do
      let fromTtoTweets :: T.Text -> Maybe TweetUserResp
          fromTtoTweets = A.decode . BL.fromStrict . TE.encodeUtf8
          dMtweets = fromTtoTweets <$> dRespT
          dTUResp = fromMaybe (TweetUserResp [] []) <$> dMtweets
      dyn_ $ ffor dTUResp $ \tuResp -> mapM_ (elTweet $ users tuResp) (tweets tuResp)     
      -- ^ dyn_ runs the Dynamic t (m ()), otherwise you'd only have a Dynamic not run.
      return ()
    el "h1" $ dynText dRespT
    return ()
  return ()

elTweet :: (DomBuilder t m, SetRoute t (R FrontendRoute) m) => [Entity User] -> Entity Tweet -> m ()
elTweet users tweet = do 
  (tweetDiv, _) <- elAttr' "div" ("class" =: "rounded-xl bg-gray-100 max-w-full w-full p-4 my-2"
                                 <> "style" =: "cursor: pointer;") $ do 
    elAttr "a" ("class" =: "text-blue-400 font-bold") $ text $ userName . entityVal $ user 
    el "h3" $ text $ tweetText $ tweet'
  let tweetId = T.pack $ show $ fromSqlKey $ entityKey tweet
  let tweetClick = domEvent Click tweetDiv 
  setRoute $ FrontendRoute_Tweet :/ tweetId <$ tweetClick
  return ()
  where 
    tweet' = entityVal tweet
    user = findRecord users $ tweetOwner tweet'

findRecord :: [Entity record] -> Schema.Key record -> Entity record
findRecord recs id = undefined
