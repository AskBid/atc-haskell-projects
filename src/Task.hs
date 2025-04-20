-- | This module provides all types and function to handle Tasks
module Task where

import Data.Time

data Task = Task
  { completed   :: Bool
  , name        :: String
  , priority    :: Priority
  , date        :: Day
  , description :: String
  } 

mkTask :: String -> IO Task
mkTask name = do
  currentDay <- utctDay <$> getCurrentTime
  return Task 
    { completed = False
    , name      = name
    , priority  = Low
    , date      = addDays 1 currentDay 
    , description = "" }

data Priority = High | Medium | Low
  deriving (Show, Enum, Read)

instance Show Task where
  show t = "\n\nTASK NAME:\n" ++ (name t) ++ "\n\n" ++ 
    "STATUS:\n" ++ (if (completed t) then "un" else "") ++ "completed" ++ "\n\n" ++
    "DESCRIPTION:\n" ++ (description t) ++ "\n----------------"

-- | Custom instantiation to keep the equality to the identity attributes only.
instance Eq Task where
  (Task completed name _ _ _) == (Task completed' name' _ _ _) = 
    completed == completed' && name' == name

-- | getTask finds a task in the todo list @[Task]@.
--   It takes a predicate function so that the filter is customisable with various equalities.
getTask :: (Task -> Bool) -> [Task] -> Either String Task
getTask predicate ts = case filter predicate ts of
  []    -> Left $ "No matching task was found."
  (t:_) -> Right t

-- | replaceTask replaces a task modified after finding it with getTask. 
--   It keeps the list order while modifying the Task.
--   It takes the old Task, the new Task to replace it with and the original todo list.
-- >>> replaceTask oldtask (Task True (name oldtask) (description oldtask) ... ) todoList
replaceTask :: Task -> Task -> [Task] -> [Task]
replaceTask _ _ [] = []
replaceTask old new (t:ts)
  | old == t  = new:ts
  | otherwise = t:(replaceTask old new ts)

-- | deleteTask takes a Task to be deleted from a list @[Task]@
deleteTask :: Task -> [Task] -> [Task]
deleteTask _ [] = []
deleteTask task (t:ts)
  | task == t = ts
  | otherwise = t:(deleteTask task ts)
