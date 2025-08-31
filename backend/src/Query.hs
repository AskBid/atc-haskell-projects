module Query where 

import Data.Text 
import qualified Database.Beam.Postgres as P
import Database.Beam
import qualified Database.Beam.Postgres.Conduit as PC
import qualified Data.Conduit.List as CL
import Data.Conduit
import Control.Monad.Trans.Resource (runResourceT)

import Schema
import Common.Api
import Migration

-- | Run a Beam query and collect all results into a list.
conduitQuery 
  :: P.Connection 
  -> (a -> Q P.Postgres ChatDB QBaseScope (UserT (QExpr P.Postgres QBaseScope))) 
  -> a 
  -> IO [User]
conduitQuery conn query name =
  runResourceT $
    runConduit $
      PC.streamingRunSelect conn (select (query name))
        -- ^ ConduitT () a m ()
        -- Think of it like: “Here’s a stream of rows (Users), you can consume them however you like.”
        .| CL.consume   
        -- ^ collect all rows into a list.

queryUserByName :: Text -> Q P.Postgres ChatDB s (UserT (QExpr P.Postgres s))
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

queryUserCredentials :: Credentials -> Q P.Postgres ChatDB s (UserT (QExpr P.Postgres s))
queryUserCredentials (Credentials usr pwd) = do
  u <- all_ (userTable chatDB)
  guard_ (_userName u ==. val_ usr)
  guard_ (_userPwd u ==. val_ pwd)
  pure u
