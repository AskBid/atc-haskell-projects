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
import Database.Beam.Migrate
import Database.Beam.Migrate.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
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
  , _messageBody :: C f Text
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

data ChatDB f = ChatDB
  { userTable    :: f (TableEntity UserT)
  -- , messageTable :: f (TableEntity MessageT)
  } deriving (Generic)
-- ^ notice that the wrapper f here is not the same as the one for Columnar
--   A table is a "row type" with each column wrapped in something 
--   (Identity for real values, Nullable for optional).
--   A database is a "schema type" with each table wrapped in something 
--   (DatabaseEntity be for actual DB metadata, or maybe something else for migrations).
instance Database be ChatDB
-- ^ When you write:
--   instance Database be DatabaseSchema
--   you’re saying:
--     For any backend be, my schema type DatabaseSchema is a valid Beam Database schema.
--     So it works generically for SQLite, Postgres, etc., without you having to make 
--     separate instances.
--   The be parameter is tied to the Database typeclass, not to the schema itself.








-- migration :: Migration Postgres (CheckedDatabaseSettings Postgres DatabaseSchema)
-- migration = do
--   user <- createTable "user"
--     (User
--       (field "id" int notNull)
--       (field "name" text notNull unique) 
--       (field "pwd" text notNull)
--     )
--   message <- createTable "message"  
--     (Message
--       (field "id" int notNull)
--       (field "body" text notNull)
--       (field "timestamp" (maybeType timestamp))
--     )
--   pure (DatabaseSchema user message)
--
-- dbSettings :: DatabaseSettings Postgres DatabaseSchema
-- dbSettings = defaultDbSettings
--   `withDbModification` dbModification
--       { userTable = setEntityName "user"
--       , messageTable = setEntityName "message"
--       }
--
-- populateUsers :: Connection -> IO ()
-- populateUsers conn = runBeamPostgres conn $ 
--   runInsert $ insertOnConflict (userTable dbSettings)
--     (insertValues 
--       [ User 1 "sergio" "pwd"
--       , User 2 "alice" "alice"
--       , User 3 "bob" "bob"
--       , User 4 "mario" "mario"
--       , User 5 "luigi" "luigi"
--       ])
--     anyConflict onConflictDoNothing
--
-- populateMessages :: Connection -> IO ()
-- populateMessages conn = runBeamPostgres conn $ 
--   runInsert $ insertOnConflict (messageTable dbSettings)
--     (insertValues 
--       [ Message 1 "Dummy first message." Nothing
--       , Message 2 "Dummy second message." Nothing
--       ])
--     anyConflict onConflictDoNothing
--
