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
  deriving (Show, Generic, FromJSON, ToJSON)
