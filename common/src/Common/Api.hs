{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Api where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.Persist.Types

import Schema

data Credentials = Credentials
  { username :: Text
  , password :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

-- | Type for frontend, tweets are followed from a list of users that covers
--   all the owners of all tweets.
--   We also carry the tweet all the replies reply to in case the query is for 
--   replies rather than main posts.
data TweetsOwnersResp = TweetsOwnersResp
  { tweets :: [Entity Tweet]
  , users :: [Entity User]
  , parentTweet :: Maybe Tweet
  } deriving (Show, Generic, FromJSON, ToJSON)
