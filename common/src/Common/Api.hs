module Common.Api where

import Data.Text
import Data.Time
import Data.Aeson

import Schema

data WSMessage 
  = ChatMessage
      { message :: Text
      , user :: User
      , timestamp :: UTCTime
      }
  | ConnectedClients [User]
