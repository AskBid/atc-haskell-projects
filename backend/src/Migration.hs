{-# LANGUAGE OverloadedStrings #-}

module Migration where

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Postgres.Full
import qualified Database.Beam.Postgres.Migrate as PG

import Schema

-- | All the major functions you'll need to actually write migrations are in 
--   Database.Beam.Migrate.SQL.Tables.
initialSetup :: Migration Postgres (CheckedDatabaseSettings Postgres ChatDB)
initialSetup = ChatDB 
  <$> (createTable "users" $ User
         { _userId = field "id" int notNull unique
         , _userName = field "name" (varchar (Just 20)) notNull unique
         , _userPwd = field "password" (varchar (Just 20)) notNull 
         }) 
  <*> (createTable "messages" $ Message
         { _messageId = field "id" int notNull unique
         , _messageBody = field "body" (varchar (Just 200))
         , _messageTimestamp = field "timestamp" (maybeType timestamp)
         , _messageOwner = UserId (field "owner_id" int notNull)
         -- ^ beam keeps the concept of a primary key separate from the definition of a column.
         })
  <*> (createTable "privates" $ Private
         { _privateMessage = MessageId (field "message_id" int notNull)
         , _privateRecipient = UserId (field "user_id" int notNull)  
         -- ^ beam keeps the concept of a primary key separate from the definition of a column.
         })
  -- ^ createTable :: Text -> TableSchema be table ->
  --   -> Migration be (CheckedDatabaseEntity be db (TableEntity table))

initialSetupStep :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres ChatDB)
initialSetupStep = migrationStep "initial_setup" (const initialSetup)

----------
-- Actually running the migration.
----------

-- |  Beam's simple migration runner, by default, prevents destructive operations 
--    like dropping tables to avoid accidental data loss. let's ovveride it.
allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive = defaultUpToDateHooks { runIrreversibleHook = pure True }

migrateDB :: Connection
          -> IO (Maybe (CheckedDatabaseSettings Postgres ChatDB))
migrateDB conn = runBeamPostgresDebug putStrLn conn $
  bringUpToDateWithHooks
    allowDestructive
    PG.migrationBackend
    initialSetupStep

-- | we require unchek DatabaseSettings to run queries.
chatDB :: DatabaseSettings Postgres ChatDB
chatDB = unCheckDatabase $ evaluateDatabase initialSetupStep

-- exampleQuery :: Connection -> IO [UserT Identity]
-- exampleQuery conn = runBeamPostgres conn $
--   runSelectReturningList $
--     select (all_ (dbFlowers flowerDB))
populateUsers :: Connection -> IO ()
populateUsers conn = runBeamPostgres conn $ 
  runInsert $ insertOnConflict (userTable chatDB)
    (insertValues 
      [ User 1 "sergio" "pwd"
      , User 2 "alice" "alice"
      , User 3 "bob" "bob"
      , User 4 "mario" "mario"
      , User 5 "luigi" "luigi"
      ])
    anyConflict onConflictDoNothing

populateMessages :: Connection -> IO ()
populateMessages conn = runBeamPostgres conn $ 
  runInsert $ insertOnConflict (messageTable chatDB)
    (insertValues 
      [ Message 1 "Dummy first message." Nothing (UserId 1)
      , Message 2 "Dummy second message." Nothing (UserId 2)
      ])
    anyConflict onConflictDoNothing
