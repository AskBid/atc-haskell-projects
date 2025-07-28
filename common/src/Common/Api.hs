module Common.Api where

import Data.Text
import Data.Time
import Data.Aeson

import Schema

data WSMessage 
  = Message
  | ConnectedClients [User]
