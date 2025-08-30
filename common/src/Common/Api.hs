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
  | NoMessage
  deriving (Show, Generic, FromJSON, ToJSON)

data Credentials = Credentials 
  { username :: Text
  , password :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | need this to be able to send only text messages from backend while
--   using common Aeson methods that process responses with actual types.
--   in other words it makes Text encodable with Aeson.
data BackendResponse = BackendResponse
  { textOnly :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

data FEMessage = FEMessage
  { wmpId        :: Maybe Int
  , wmpBody      :: Text
  , wmpTimestamp :: Maybe LocalTime
  , wmpOwner     :: Text
  , wmpTarget    :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
