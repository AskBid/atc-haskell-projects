{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass  #-}

module Common.Api where

import Data.Text
import Data.Time
import Data.Aeson
import GHC.Generics (Generic)

import Schema

data WSMessage 
  = NewMessage Message
  | ConnectedClients [User]
  | UserExist Text
  | NoUser
  deriving (Show, Generic, FromJSON, ToJSON)

data Credentials = Credentials 
  { username :: Text
  , password :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)
