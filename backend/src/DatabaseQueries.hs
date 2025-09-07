{-# LANGUAGE OverloadedStrings          #-}

module DatabaseQueries where

import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (catMaybes)
import Control.Monad.Reader

import Common.MyFunctions
import Common.Api
import Schema

-- | checks if the data in the LoginReq is a valid user in the database.
sqlUserPwdExist :: MonadIO m => Credentials -> m (Maybe (Entity User))
sqlUserPwdExist lr = do
  liftIO $ runSqlite myDB $ do
    mEUser <- selectFirst [UserName ==. (username lr), UserPwd ==. (password lr)] []
    return mEUser

getPosts :: IO TweetUserResp
getPosts = do 
  posts <- runSqlite myDB $ selectList [TweetReplyTo ==. Nothing] [Desc TweetCreatedAt]
  users <- catMaybes <$> mapM findUsers posts
  return $ TweetUserResp posts users

findUsers :: Entity Tweet -> IO (Maybe (Entity User))
findUsers et = do 
  mEUser <- runSqlite myDB $ selectFirst [UserId ==. id] []
  return mEUser
  where 
    id = tweetOwner $ entityVal et 

insertTweet :: Tweet -> IO (Key Tweet)
insertTweet tweet = do 
  k <- runSqlite myDB $ insert tweet
  return k

insertUser :: User -> IO (Key User)
insertUser user = do 
  k <- runSqlite myDB $ insert user
  return k

getPostReplies :: Entity Tweet -> IO [Entity Tweet]
getPostReplies tweet = do 
  replies <- runSqlite myDB $ selectList [TweetReplyTo ==. Just (entityKey tweet)] [Desc TweetCreatedAt]
  return replies
