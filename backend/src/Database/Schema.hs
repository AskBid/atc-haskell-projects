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

module Database.Schema where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Beam
import GHC.Int (Int32)
import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial)

-- User
data UserT f = User
  { _userId   :: C f (SqlSerial Int32)
  , _userName :: C f Text
  , _userPwd  :: C f Text
  } deriving stock Generic
    deriving anyclass Beamable
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
deriving stock instance Show User
deriving stock instance Eq User
deriving anyclass instance FromJSON User
deriving anyclass instance ToJSON User
deriving stock instance Show (PrimaryKey UserT Identity)
deriving stock instance Eq   (PrimaryKey UserT Identity)
deriving anyclass instance FromJSON (PrimaryKey UserT Identity)
deriving anyclass instance ToJSON (PrimaryKey UserT Identity)

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f (SqlSerial Int32)) 
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = UserId . _userId

----------
-- Message
data MessageT f = Message
  { _messageId   :: C f (SqlSerial Int32)
  , _messageBody :: C f Text
  , _messageTimestamp :: C f (Maybe LocalTime)
  , _messageOwner :: PrimaryKey UserT f
  } deriving stock Generic
    deriving anyclass Beamable

type Message = MessageT Identity
type MessageId = PrimaryKey MessageT Identity

deriving stock instance Show Message
deriving stock instance Eq Message
deriving anyclass instance FromJSON Message
deriving anyclass instance ToJSON Message

instance Table MessageT where
  data PrimaryKey MessageT f = MessageId (C f (SqlSerial Int32)) 
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = MessageId . _messageId

----------
-- Private
data PrivateT f = Private
  { _privateMessage :: PrimaryKey MessageT f 
  , _privateRecipient :: PrimaryKey UserT f 
  } deriving stock Generic
    deriving anyclass Beamable

type Private = PrivateT Identity
type PrivateId = PrimaryKey PrivateT Identity

instance Table PrivateT where
  data PrimaryKey PrivateT f = PrivateId (PrimaryKey MessageT f) (PrimaryKey UserT f)
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = PrivateId <$> _privateMessage <*> _privateRecipient
  -- ^ primaryKey :: table column -> PrimaryKey table column
  -- function that extracts the “key fields” from a row.
  -- like: primaryKey (Private m u) = PrivateId m u

-----------
-- Database
data ChatDB f = ChatDB
  { userTable    :: f (TableEntity UserT)
  , messageTable :: f (TableEntity MessageT)
  , privateTable :: f (TableEntity PrivateT)
  } deriving stock Generic
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

