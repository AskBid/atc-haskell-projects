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
    createdAt UTCTime Maybe
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
  _  <- insertBy $ User "alice" "alice" []
  _  <- insertBy $ User "bob" "bob" []
  _  <- insertBy $ User "sergio" "pwd" []
  _  <- insertBy $ User "mario" "mario" []
  _  <- insertBy $ User "luigi" "luigi" []
  _  <- insertBy $ User "raoul" "raoul" []

  _  <- insertBy $ Tweet "Ciao Mondo! my first tweet!" 
    Nothing -- replying to 
    (toSqlKey 1) $ -- owner
      Just $ UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0)

  t2  <- insertBy $ Tweet "Am I the second?" 
    Nothing -- replying to 
    (toSqlKey 2) $ -- owner
      Just $ UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 100)

  t3 <- insertBy $ Tweet "The S&P500 to break ATH!!" 
    Nothing -- replying to 
    (toSqlKey 3) $ -- owner
      Just $ UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1100)

  _  <- insertBy $ Tweet "yup, I was first" 
    (Just $ key' t2) -- replying to 
    (toSqlKey 1) $ -- owner
      Just $ UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1500)

  r1 <- insertBy $ Tweet "No way! CPI next week will take markets down!" 
    (Just $ key' t3) -- replying to 
    (toSqlKey 4) $ -- owner
      Just $ UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1500)

  t4 <- insertBy $ Tweet "I agree, the FED seems changing tone." 
    (Just $ key' t3) -- replying to 
    (toSqlKey 5) $ -- owner
      Just $ UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1600)

  t5  <- insertBy $ Tweet "Yes I was!" 
    (Just $ key' t2) -- replying to 
    (toSqlKey 2) $ -- owner
      Just $ UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1700)

  _  <- insertBy $ Tweet "Well done, this is the first reply of a reply." 
    (Just $ key' t5) -- replying to 
    (toSqlKey 6) $ -- owner
      Just $ UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1000)

  _  <- insertBy $ Tweet "Commodities seem to indicate a deflationary environment!" 
    (Just $ key' r1) -- replying to 
    (toSqlKey 6) $ -- owner
      Just $ UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1000)
  -- ^ need @insertBy@ rather than @insert_@ because we need to check if record 
  -- already exist from previously generated DataBase.
  return ()
  where 
    key' (Left _)  = toSqlKey 1
    key' (Right k) = k

dummyEntityUser :: Entity User
dummyEntityUser = Entity (toSqlKey 1) (User "" "" [])
