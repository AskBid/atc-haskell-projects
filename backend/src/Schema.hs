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
{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}

module Schema where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Beam
import qualified Database.Beam.AutoMigrate as AM
import Database.Beam.Postgres
import Data.Proxy (Proxy(..))
import GHC.Int (Int32)


data UserT f = User
  { _userId   :: C f Int32
  , _userName :: C f Text
  , _userPwd  :: C f Text
  } deriving (Generic, Beamable)
-- ^ basically every attribute is a @Columnar@ also said @C@ that is a Data Family in 
--   which one argument needs a type to give back a concrete type, so it is a container
--   (Type -> Type) or (* -> *) like Identity or Nullable (it's Type Constructor)
--   and @a@ that is the actual values type we want for our Columns.

type User = UserT Identity
-- ^ not to use UserT specifick for its true form (Identity) all the time 
--   we shortend the userT Identity to User. Notice how User here does not 
--   collide with User constructor in UserT, cus onw is in the Type world the
--   other in the values world.. and they actually match as an data User = User {..} 
type UserId = PrimaryKey UserT Identity
-- ^ like saying: I want a convenient short name UserId for the concrete 
--   primary-key type of the UserT table, in its real-data form.”

deriving instance Show User
deriving instance Eq User
deriving instance FromJSON User
deriving instance ToJSON User

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f Int32) 
    deriving (Generic, Beamable)
  primaryKey = UserId . _userId



data MessageT f = Message
  { _messageId   :: C f Int32
  , _messageText :: C f Text
  , _messageTimestamp :: C f (Maybe LocalTime)
  } deriving (Generic, Beamable)

type Message = MessageT Identity
type MessageId = PrimaryKey MessageT Identity

deriving instance Show Message
deriving instance Eq Message
deriving instance FromJSON Message
deriving instance ToJSON Message

instance Table MessageT where
  data PrimaryKey MessageT f = MessageId (C f Int32) 
    deriving (Generic, Beamable)
  primaryKey = MessageId . _messageId

data DatabaseSchema f = DatabaseSchema
  { userTable    :: f (TableEntity UserT)
  , messageTable :: f (TableEntity MessageT)
  } deriving (Generic)
-- ^ notice that the wrapper f here is not the same as the one for Columnar
--   A table is a "row type" with each column wrapped in something 
--   (Identity for real values, Nullable for optional).
--   A database is a "schema type" with each table wrapped in something 
--   (DatabaseEntity be for actual DB metadata, or maybe something else for migrations).
instance Database be DatabaseSchema
-- ^ When you write:
--   instance Database be DatabaseSchema
--   you’re saying:
--     For any backend be, my schema type DatabaseSchema is a valid Beam Database schema.
--     So it works generically for SQLite, Postgres, etc., without you having to make 
--     separate instances.
--   The be parameter is tied to the Database typeclass, not to the schema itself.

db :: DatabaseSettings Postgres DatabaseSchema
db = defaultDbSettings

dbSettings :: AM.AnnotatedDatabaseSettings Postgres DatabaseSchema
dbSettings = (AM.defaultAnnotatedDbSettings db) 

schema :: AM.Schema
schema = AM.fromAnnotatedDbSettings dbSettings (Proxy :: Proxy ('[] :: [AM.Annotation]))

connInfo :: ConnectInfo
connInfo = ConnectInfo
  { connectHost = "localhost" -- :: String	 
  , connectPort = 5432 -- :: Word16	 
  , connectUser = "atc_user" -- :: String	 
  , connectPassword = "atcpassword" -- :: String	 
  , connectDatabase = "atc_db" -- :: String	 
  }

populateUsers :: Connection -> IO ()
populateUsers conn = runBeamPostgres conn $ 
  runInsert $ insert (userTable db) $ insertValues 
    [ User { _userId   = 1 
           , _userName = "sergio"
           , _userPwd  = "pwd"
           }
    , User { _userId   = 2 
           , _userName = "alice"
           , _userPwd  = "alice"
           } 
    , User { _userId   = 3 
           , _userName = "bob"
           , _userPwd  = "bob"
           } 
    , User { _userId   = 4 
           , _userName = "mario"
           , _userPwd  = "mario"
           }
    , User { _userId   = 5 
           , _userName = "luigi"
           , _userPwd  = "luigi"
           }
    ]

populateMessages :: Connection -> IO ()
populateMessages conn = runBeamPostgres conn $ 
  runInsert $ insert (messageTable db) $ insertValues 
    [ Message { _messageId = 1 
              , _messageText = "Dummy first message."
              , _messageTimestamp  = Nothing
              }
    , Message { _messageId = 2 
              , _messageText = "Dummy second message."
              , _messageTimestamp = Nothing
              } 
    ]

-- populateDB :: MonadIO m => SqlPersistT m ()
-- populateDB = do
--   runMigration migrateAll
--   _  <- insertBy $ User "alice" "alice"
--   _  <- insertBy $ User "bob" "bob"
--   _  <- insertBy $ User "sergio" "pwd"
--   _  <- insertBy $ User "mario" "mario"
--   _  <- insertBy $ User "luigi" "luigi"
--   _  <- insertBy $ User "raoul" "raoul"
--   return ()
