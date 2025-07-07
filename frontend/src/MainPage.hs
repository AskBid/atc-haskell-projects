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
-- import Control.Monad.Trans (lift)
-- import Control.Monad.IO.Class (liftIO)
import Schema
import Data.Maybe
import Database.Persist.Sql

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
      let fromTtoTweets :: T.Text -> Maybe [Entity Tweet]
          fromTtoTweets = A.decode . BL.fromStrict . TE.encodeUtf8
          dMtweets = fromTtoTweets <$> dRespT
      dyn_ $ mapM_ dynTweet . fromMaybe [] <$> dMtweets
      -- ^ dyn_ runs the Dynamic t (m ()), otherwise you'd only have a Dynamic not run.
      return ()
    el "h1" $ dynText dRespT
    return ()
  return ()

dynTweet :: DomBuilder t m => Entity Tweet -> m ()
dynTweet tweet = do 
  elAttr "div" ("class" =: "rounded-xl bg-gray-100 max-w-full w-full p-4 my-2") $ do 
    elAttr "a" ("class" =: "text-blue-400 font-bold") $ text "user_here"
    el "h3" $ text $ tweetText $ entityVal tweet
  return ()


