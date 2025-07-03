{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}

module Schema where

import Database.Persist.TH
import Database.Persist
import Database.Persist.Sqlite
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Common.MyFunctions
import Common.Api

import Debug.Trace

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Tweet
    text Text
    replyTo TweetId Maybe
    owner UserId
    UniqueTweet text owner
    deriving Show

  User
    name Text
    pwd Text
    follows [UserId]
    UniqueName name
    deriving Show Generic FromJSON ToJSON
|]

myDB :: Text
myDB = "Xs.db"

-- | checks if the data in the LoginReq is a valid user in the database.
loginDB :: MonadIO m => LoginReq -> m (Maybe User)
loginDB lr = do
  liftIO $ runSqlite myDB $ do
    user <- selectList [UserName ==. (username lr), UserPwd ==. (password lr)] []
    return $ entityVal <$> headSafe user
  where 
    name = username lr 
    pwd = password lr

getPosts :: IO [Entity Tweet]
getPosts = runSqlite myDB $ selectList [TweetReplyTo ==. Nothing] []

printSQL :: Show a => IO a -> IO ()
printSQL query = do 
  twts <- query
  print twts
