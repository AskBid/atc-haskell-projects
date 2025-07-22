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
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Control.Monad.IO.Class (MonadIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    name Text
    pwd Text
    UniqueName name
    deriving Show Generic FromJSON ToJSON
|]

-- | this makes it eprsistent, use :memory: instead of Xs.db otherwise 
myDB :: Text
myDB = "Xs.db"

populateDB :: MonadIO m => SqlPersistT m ()
populateDB = do
  runMigration migrateAll
  _  <- insertBy $ User "alice" "alice"
  _  <- insertBy $ User "bob" "bob"
  _  <- insertBy $ User "sergio" "pwd"
  _  <- insertBy $ User "mario" "mario"
  _  <- insertBy $ User "luigi" "luigi"
  _  <- insertBy $ User "raoul" "raoul"
  return ()
