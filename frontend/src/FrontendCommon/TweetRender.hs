{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE FlexibleContexts  #-}

module FrontendCommon.TweetRender where

import Common.Route 
import Reflex.Dom.Core
import Obelisk.Route
import Obelisk.Route.Frontend
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Language.Javascript.JSaddle (MonadJSM)
import Database.Persist.Sql
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Data.Maybe

import Schema
import Common.Api
import FrontendCommon.Common
import Common.MyFunctions
-- | given an Entity Tweet and a list [Entity User] renders the tweet 
--   with the related owner User.
--   the logic is in the route Api_Posts that together with a list 
--   [Entity Tweet] returns a list [Entity User] with all the related 
--   owner of all [Entity Tweet] returned.
elTweet 
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , MonadHold t m
     , MonadFix m
     , MonadIO m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , PostBuild t m
     ) 
  => [Entity User] 
  -> Entity Tweet 
  -> m ()
elTweet users' tweet = do 

  let usernameT = fromMaybe "" $ userName . entityVal <$> user
      url = getUrl $ 
        FullRoute_Frontend (ObeliskRoute_App FrontendRoute_Profile) :/ usernameT

  elClass "div" (containerStyle <> containerBorder) $ do

    elAttr "a" ("class" =: userStyle <> "href" =: url) $ 
      text usernameT

    (tweetTextDiv, _) <- elAttr' 
        "div" 
        ( "style" =: pointer
        ) $ text $ tweetText $ tweet'

    tweetReplyLink <- elClass "div" "flex" $ do
      elClass "div" "flex-1" blank
      (tweetReplyLink, _) <- elAttr' 
          "div" 
          ( "style" =: pointer 
          <> "class" =: replyStyle
          ) $ text "reply\x1F5E8"
      return tweetReplyLink

    let tweetId = T.pack $ show $ fromSqlKey $ entityKey tweet
        evTextClick = domEvent Click tweetTextDiv 
        evReplyClick = domEvent Click tweetReplyLink 
        url4replies = getUrl $ 
          FullRoute_Backend 
          BackendRoute_Api :/
          Api_PostReplies tweetId
    dToggleReplies <- toggle False evTextClick
     
    let eToggleReplies = updated dToggleReplies
        eShowReplies = ffilter id eToggleReplies
    --     eHideReplies = ffilter (not id) eToggleReplies
    --
    emTweetUserReplies <- getAndDecode $ url4replies <$ eShowReplies

    dyn_ $ ffor dToggleReplies $ \toggleReplies -> 
        if not toggleReplies
        then blank
        else widgetHold_ (tweetTabMsg "Loading replies...") $ 
          ffor emTweetUserReplies $ \case
            Nothing -> el "div" $ text errReplies
            Just tuReplies -> do 
              elTweetsList tuReplies
        
    setRoute $ FrontendRoute_Tweet :/ tweetId <$ evReplyClick
  where 
    tweet' = entityVal tweet
    user = findInEntityList users' $ tweetOwner tweet'

    errReplies = "404. No replies retrieved."

    replyStyle      = "text-right text-xs text-gray-400 underline"
    userStyle       = "text-blue-400 font-bold"
    containerStyle  = "rounded-xl bg-gray-100 max-w-full w-full p-2 my-4 "
    containerBorder = "border-4 border-white"
    pointer         = "cursor: pointer;"

elTweetsList 
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , MonadHold t m
     , MonadFix m
     , MonadIO m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , PostBuild t m
     ) 
  => TweetsOwnersResp 
  -> m ()
elTweetsList tur = 
  case tur of 
    TweetsOwnersResp [] _ _         -> tweetTabMsg "Nothing to see here."
    TweetsOwnersResp tweets' users' _ -> mapM_ (elTweet $ users') tweets'

tweetTabMsg 
  :: DomBuilder t m 
  => T.Text 
  -> m ()
tweetTabMsg msg = elClass "div" css $ text msg
  where 
    css = "text-gray-400 text-sm"

-- | finds the record relative to an id/key given a list of Entity and the key.
findInEntityList 
  :: Eq (Schema.Key record) 
  =>  [Entity record] 
  -> Schema.Key record 
  -> Maybe (Entity record)
findInEntityList recs id' = headSafe $ filter (\(Entity k _) -> k == id') recs
-- ^ Even though Persistent derives Eq for every Key MyEntity, the compiler doesn't know 
--   that for all record types unless you say so.

requestAndListTweets 
  :: ( PerformEvent t m
     , MonadJSM (Performable m)
     , TriggerEvent t m
     , MonadHold t m
     , PostBuild t m
     , MonadIO m
     , MonadFix m
     , DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     )
  => Event t a 
  -> R (FullRoute BackendRoute FrontendRoute) 
  ->  m (Dynamic t (Maybe TweetsOwnersResp))
requestAndListTweets event routeQueryTweets = do 
  el "div" $ do
    let xhrGetPosts = XhrRequest
          { _xhrRequest_method = "GET"
          , _xhrRequest_url = getUrl routeQueryTweets
          , _xhrRequest_config = def & xhrRequestConfig_withCredentials .~ True
          }

    evGetPostsResp <- performRequestAsync $ xhrGetPosts <$ event
    
    dTweetsOwnersResp <- holdDyn Nothing $ ffor evGetPostsResp $ \resp ->
      case _xhrResponse_responseText resp of
        Nothing   -> Nothing
        Just textResp -> A.decode . BL.fromStrict . TE.encodeUtf8 $ textResp

    dyn_ $ ffor dTweetsOwnersResp $ \case
      Nothing     -> el "div" $ text "something went wrong."
      Just tuResp -> elTweetsList tuResp

    return (dTweetsOwnersResp)

mkTweet :: Maybe T.Text -> Maybe (Entity User) -> T.Text -> Maybe Tweet
mkTweet _ Nothing _ = Nothing
mkTweet Nothing (Just (Entity usrKey _)) areaText = 
  Just $ Tweet 
    { tweetText = areaText
    , tweetReplyTo = Nothing
    , tweetOwner = usrKey
    , tweetCreatedAt = Nothing
    }
mkTweet (Just tId) (Just (Entity usrKey _)) areaText =
  case textToInt64 tId of
    Nothing       -> Nothing
    Just replyKey -> Just $ Tweet
      { tweetText = areaText
      , tweetReplyTo = Just $ toSqlKey replyKey
      , tweetOwner = usrKey
      , tweetCreatedAt = Nothing
      }
