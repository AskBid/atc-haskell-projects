{-# LANGUAGE FlexibleContexts #-}

module Query where 

import Data.Text 
import Database.Beam
import qualified Database.Beam.Postgres.Conduit as PC
import qualified Data.Conduit.List as CL
import Data.Conduit
import Control.Monad.Trans.Resource (runResourceT)
import Database.Beam.Postgres (Connection, runBeamPostgres, Postgres)
import Database.Beam.Postgres.Full (insertOnConflict)
import Database.Beam (Beamable, FromBackendRow) 

import Schema
import Common.Api
import Migration
import DatabaseFrontendBridge

-- | Run a Beam query and collect all results into a list.
conduitQuery 
  :: ( Beamable table
     , FromBackendRow Postgres (table Identity)
     )
  => Connection 
  -> (a -> Q Postgres ChatDB QBaseScope (table (QExpr Postgres QBaseScope))) 
  -> a 
  -> IO [table Identity]
conduitQuery conn query value =
  runResourceT $
    runConduit $
      PC.streamingRunSelect conn (select (query value))
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

queryUserCredentials :: Credentials -> Q Postgres ChatDB s (UserT (QExpr Postgres s))
queryUserCredentials (Credentials usr pwd) = do
  u <- all_ (userTable chatDB)
  guard_ (_userName u ==. val_ usr)
  guard_ (_userPwd u ==. val_ pwd)
  pure u

-- | we need to use @insertExpression@ because we don't know the IDs as 
--   we are using serial IDs, therefore we can't use concrete Haskell Values.
insertMessage :: Connection -> Text -> UserId -> IO ()
insertMessage conn body ownerId =
  runBeamPostgres conn $
    runInsert $ insert (messageTable chatDB) $
      insertExpressions
        [ Message default_ (val_ body) nothing_ (val_ ownerId) ]

insertFromFEMessage :: FEMessage -> Connection -> IO ()
insertFromFEMessage fem conn = do 
  users <- conduitQuery conn queryUserByName (femOwner fem)
  case users of
    []     -> undefined
    (u:_) -> insertMessage conn (femBody fem) (UserId $ _userId u)
