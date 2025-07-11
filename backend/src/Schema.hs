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
import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Tweet
    text Text
    replyTo TweetId Maybe
    owner UserId
    createdAt UTCTime default=now()
    UniqueTweet text owner
    deriving Show Generic FromJSON ToJSON

  User
    name Text
    pwd Text
    follows [UserId]
    UniqueName name
    deriving Show Generic FromJSON ToJSON
|]

-- | To have database ids in the frontend better send the Entity Tweet as XhrResponse rather
--   than Tweet alone. this instances enable Entity Tweet to port from and to JSON.
instance FromJSON (Entity Tweet) where
    parseJSON = entityIdFromJSON

instance ToJSON (Entity Tweet) where
    toJSON = entityIdToJSON

instance FromJSON (Entity User) where
    parseJSON = entityIdFromJSON

instance ToJSON (Entity User) where
    toJSON = entityIdToJSON

-- | this makes it eprsistent, use :memory: instead of Xs.db otherwise 
myDB :: Text
myDB = "Xs.db"

populateDB :: MonadIO m => SqlPersistT m ()
populateDB = do
  runMigration migrateAll
  _  <- insertBy $ User "alice" "alice123" []
  _  <- insertBy $ User "bob" "bob456" []
  _  <- insertBy $ User "sergio" "pwd" []
  _  <- insertBy $ User "mario" "alice123" []
  _  <- insertBy $ User "luigi" "bob456" []
  _  <- insertBy $ User "raoul" "pwd" []
  _  <- insertBy $ Tweet "Ciao Mondo! my first tweet!" Nothing (toSqlKey 1) $ 
    UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0)
  t2  <- insertBy $ Tweet "Am I the second?" Nothing (toSqlKey 2) $
    UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 100)
  t3 <- insertBy $ Tweet "the laggard I guess?" Nothing (toSqlKey 3) $
    UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1100)
  _  <- insertBy $ Tweet "yup, I was first" (Just $ key' t3) (toSqlKey 1) $
    UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1500)
  _  <- insertBy $ Tweet "y8888888" (Just $ key' t3) (toSqlKey 4) $
    UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1500)
  t4  <- insertBy $ Tweet "t88878787, replyy" (Just $ key' t3) (toSqlKey 5) $
    UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1600)
  t5  <- insertBy $ Tweet "tdsfdsfdsf787, replyy" (Just $ key' t2) (toSqlKey 2) $
    UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1700)
  _  <- insertBy $ Tweet "replyy of replyy woo" (Just $ key' t5) (toSqlKey 6) $
    UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1000)
  -- ^ need @insertBy@ rather than @insert_@ because we need to check if record already exist
  --   from previously generated DataBase.
  return ()
  where 
    key' (Left _)  = toSqlKey 1
    key' (Right k) = k

dummyEntityUser :: Entity User
dummyEntityUser = Entity (toSqlKey 1) (User "" "" [])
