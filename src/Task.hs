-- | This module provides all types and function to handle Tasks
module Task where

data Task = Task
  { completed :: Bool
  , name      :: String
  , description :: String
  -- , priority :: Priority
  } 

data Priority = High | Medium | Low
  deriving (Show, Enum)

instance Show Task where
  show t = "\nTASK NAME: " ++ (name t) ++ "\n" ++ 
    "STATUS: " ++ (if (completed t) then "un" else "") ++ "completed" ++ "\n" ++
    "DESCRIPTION:\n" ++ (description t) ++ "\n----------------"

-- | Custom instantiation to keep the equality to the identity attributes only.
instance Eq Task where
  (Task completed name _) == (Task completed' name' _) = 
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

testTs :: [Task]
testTs = [Task False "task1" "desc1", Task True "task2" "desc2", Task False "" ""]



