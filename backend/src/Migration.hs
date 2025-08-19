{-# LANGUAGE OverloadedStrings #-}

module Migration where

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Backend
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate as PG

import Schema

-- | All the major functions you'll need to actually write migrations are in 
--   Database.Beam.Migrate.SQL.Tables.
initialSetup :: Migration Postgres (CheckedDatabaseSettings Postgres ChatDB)
initialSetup = ChatDB <$>
  ( createTable "users" $ User
      { _userId = field "id" int notNull unique
      , _userName = field "name" (varchar (Just 20)) notNull unique
      , _userPwd = field "password" (varchar (Just 20)) notNull 
      }
  )  
  -- ^ createTable :: Text -> TableSchema be table ->
  --   -> Migration be (CheckedDatabaseEntity be db (TableEntity table))

initialSetupStep :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres ChatDB)
initialSetupStep = migrationStep "initial_setup" (const initialSetup)

----------
-- Actually running the migration.
----------

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

