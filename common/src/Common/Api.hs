module Common.Api where

import Data.Text
import Data.Time
import Data.Aeson

type UserName = Text

data ChatMessage = ChatMessage 
  { message :: Text
  , user :: UserName
  , timestamp :: UTCTime
  }
