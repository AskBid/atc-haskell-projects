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

data TweetUserResp = TweetUserResp
  { tweets :: [Entity Tweet]
  , users :: [Entity User]
  } deriving (Show, Generic, FromJSON, ToJSON)
