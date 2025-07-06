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
    deriving Show Generic FromJSON ToJSON

  User
    name Text
    pwd Text
    follows [UserId]
    UniqueName name
    deriving Show Generic FromJSON ToJSON
|]

myDB :: Text
myDB = "Xs.db"
-- ^ this makes it eprsistent, use :memory: instead of Xs.db otherwise 

-- | checks if the data in the LoginReq is a valid user in the database.
loginDB :: MonadIO m => LoginReq -> m (Maybe User)
loginDB lr = do
  liftIO $ runSqlite myDB $ do
    user <- selectList [UserName ==. (username lr), UserPwd ==. (password lr)] []
    return $ entityVal <$> headSafe user
  where 
    name = username lr 
    pwd = password lr

getPosts :: IO ([Entity Tweet], [Entity User])
getPosts = do 
  posts <- runSqlite myDB $ selectList [TweetReplyTo ==. Nothing] []
  users <- findUsers posts
  return (posts, users)

findUsers :: [Entity Tweet] -> IO [Entity User]
findUsers = undefined

getPostReplies :: Entity Tweet -> IO [Entity Tweet]
getPostReplies tweet = do 
  replies <- runSqlite myDB $ selectList [TweetReplyTo ==. Just (entityKey tweet)] []
  return replies
  
printSQL :: Show a => IO a -> IO ()
printSQL query = do 
  tweets <- query
  print tweets

populateDB :: MonadIO m => SqlPersistT m ()
populateDB = do 
  runMigration migrateAll
  alice   <- insertBy $ User "alice" "alice123" []
  bob     <- insertBy $ User "bob" "bob456" []
  sergio  <- insertBy $ User "sergio" "pwd" []
  mario   <- insertBy $ User "mario" "alice123" []
  luigi   <- insertBy $ User "luigi" "bob456" []
  raoul   <- insertBy $ User "raoul" "pwd" []
  _  <- insertBy $ Tweet "Ciao Mondo! my first tweet!" Nothing $ key' alice
  _  <- insertBy $ Tweet "Am I the second?" Nothing $ key' bob
  t3 <- insertBy $ Tweet "the laggard I guess?" Nothing $ key' sergio
  _  <- insertBy $ Tweet "yup, I was first" (Just $ key' t3) $ key' alice
  -- ^ need @insertBy@ rather than @insert_@ because we need to check if record is already existend
  --   from previously generated DataBase.
  return ()
    where 
      key' (Left _)  = toSqlKey 1
      key' (Right k) = k

