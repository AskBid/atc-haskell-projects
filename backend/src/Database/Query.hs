{-# LANGUAGE FlexibleContexts #-}

module Database.Query where 

import Data.Text 
import Database.Beam
import qualified Database.Beam.Postgres.Conduit as PC
import qualified Data.Conduit.List as CL
import Data.Conduit
import Control.Monad.Trans.Resource (runResourceT)
import Database.Beam.Postgres (Connection, runBeamPostgres, Postgres)

import Database.Schema
import Common.Api
import Database.Migration

-- | Run a Beam query and collect all results into a list.
conduitQuery 
  :: ( Beamable table
     , FromBackendRow Postgres (table Identity)
     )
  => Connection 
  -> (a -> Q Postgres ChatDB QBaseScope (table (QExpr Postgres QBaseScope))) 
  -> a 
  -> IO [table Identity]
conduitQuery conn query qValue =
  runResourceT $
    runConduit $
      PC.streamingRunSelect conn (select (query qValue))
        -- ^ ConduitT () a m ()
        -- Think of it like: “Here’s a stream of rows (Users), you can consume them however you like.”
        .| CL.consume   
        -- ^ collect all rows into a list.

queryUserByName :: Text -> Q Postgres ChatDB s (UserT (QExpr Postgres s))
-- ^ s is the query scope phantom type. 
--   to track query scoping at the type level, 
--   so you don’t accidentally mix rows from different queries or cross scope 
--   boundaries incorrectly.
--   Think of s like a unique query id at the type level.
--   Every time you start a new query (Q _ s _), GHC invents a new s.
--   That way Beam can enforce rules like:
--     You can join rows from the same scope (s matches).
--     You cannot directly compare an expression from query A with query B (s ≠ s').
--   when you “join” two tables in a query, you’re really creating a new derived scope
--   that contains columns from both tables. Conceptually, it’s like a new intermediate table
queryUserByName name = do
  u <- all_ (userTable chatDB)
  guard_ (_userName u ==. val_ name)
  pure u

queryUsersByNames :: [Text] -> Q Postgres ChatDB s (UserT (QExpr Postgres s))
queryUsersByNames names = do
  u <- all_ (userTable chatDB)
  guard_ (_userName u `in_` Prelude.map val_ names)
  pure u

queryUserCredentials :: Credentials -> Q Postgres ChatDB s (UserT (QExpr Postgres s))
queryUserCredentials (Credentials usr pwd) = do
  u <- all_ (userTable chatDB)
  guard_ (_userName u ==. val_ usr)
  guard_ (_userPwd u ==. val_ pwd)
  pure u

-- | Insert Message to DB giving attributes.
--   we need to use @insertExpression@ because we don't know the IDs as 
--   we are using serial IDs, therefore we can't use concrete Haskell Values.
insertMessage :: Connection -> Text -> UserId -> IO ()
insertMessage conn body ownerId =
  runBeamPostgres conn $
    runInsert $ insert (messageTable chatDB) $
      insertExpressions
        [ Message default_ (val_ body) nothing_ (val_ ownerId) ]
      -- TODO account for private recipients too.

-- | to insert in database from a frontend message.
insertFromFEMessage :: FEMessage -> Connection -> IO ()
insertFromFEMessage fem conn = do 
  let u = femOwner fem
  existing <- conduitQuery conn queryUserById (primaryKey u)
  case existing of
    []    -> return ()
    (u':_) -> do 
      insertMessage conn (femBody fem) (primaryKey u')
      -- ^ data PrimaryKey UserT f = UserId (C f (SqlSerial Int32))
      --   A UserId is not just the integer, it’s a wrapper around _userId.
      --   Whenever Beam expects a UserId, you must wrap _userId inside UserId.
      --   or use the primaryKey method coming from Table instance
      case femPrivate fem of
        Nothing         -> return ()
        Just _          -> do 
          -- TODO insert `Private`s 
          return ()

queryUserById :: UserId -> Q Postgres ChatDB s (UserT (QExpr Postgres s))
queryUserById uid = do
  u <- all_ (userTable chatDB)
  guard_ (primaryKey u ==. val_ uid)
  pure u
