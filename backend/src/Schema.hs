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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Tweet
  text Text
  repltyTo TweetId Maybe
  owner UserId
  deriving Show

User
  name Text
  pwd Text
  follows [UserId]
  UniqueName name
  deriving Show Generic FromJSON ToJSON
|]

-- | checks if the data in the LoginReq is a valid user in the database.
loginDB :: MonadIO m => LoginReq -> m (Maybe User)
loginDB lr = do
  liftIO $ runSqlite "Xs.db" $ do
    user <- selectList [UserName ==. (username lr), UserPwd ==. (password lr)] []
    return $ entityVal <$> headSafe user
  where 
    name = username lr 
    pwd = password lr

