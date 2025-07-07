{-# LANGUAGE OverloadedStrings          #-}

module DatabaseQueries where

import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO, MonadIO)

import Common.MyFunctions
import Common.Api
import Schema

-- | checks if the data in the LoginReq is a valid user in the database.
loginDB :: MonadIO m => LoginReq -> m (Maybe User)
loginDB lr = do
  liftIO $ runSqlite myDB $ do
    user <- selectList [UserName ==. (username lr), UserPwd ==. (password lr)] []
    return $ entityVal <$> headSafe user

getPosts :: IO ([Entity Tweet], [Entity User])
getPosts = do 
  posts <- runSqlite myDB $ selectList [TweetReplyTo ==. Nothing] []
  users <- findUsers posts
  return (posts, users)

findUsers :: [Entity Tweet] -> IO [Entity User]
findUsers = pure $ runSqlite myDB $ selectList [] []

getPostReplies :: Entity Tweet -> IO [Entity Tweet]
getPostReplies tweet = do 
  replies <- runSqlite myDB $ selectList [TweetReplyTo ==. Just (entityKey tweet)] []
  return replies
