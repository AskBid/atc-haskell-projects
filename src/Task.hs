-- | This module provides all types and function to handle Tasks
module Task where

data Task = Task
  { completed :: Bool
  , name      :: String
  , description :: String
  -- , priority :: Priority
  } deriving (Show)

data Priority = High | Medium | Low
  deriving (Show, Enum)

instance Eq Task where
  (Task completed name _) == (Task completed' name' _) = completed == completed' && name' == name

getTask :: (Task -> Bool) -> [Task] -> Either String Task
getTask predicate ts = case filter predicate ts of
  []    -> Left $ "No matching task was found."
  (t:_) -> Right t

replaceTask :: Task -> Task -> [Task] -> [Task]
replaceTask _ _ [] = []
replaceTask old new (t:ts)
  | old == t  = new:ts
  | otherwise = t:(replaceTask old new ts)

deleteTask :: Task -> [Task] -> [Task]
deleteTask _ [] = []
deleteTask task (t:ts)
  | task == t = ts
  | otherwise = t:(deleteTask task ts)

testTs :: [Task]
testTs = [Task False "task1" "desc1", Task True "task2" "desc2", Task False "" ""]



