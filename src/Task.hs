module Task where

data Task = Task
  { completed :: Bool
  , name      :: String
  , description :: String
  } deriving (Show, Eq)

getTask :: String -> [Task] -> Either String Task
getTask name ts = case filter' ts of  
  []    -> Left $ "No task " ++ name ++ " was found, or it was already completed"
  (t:_) -> Right t
  where 
    filter' = filter (\(Task False name' _) -> name' == name)

replaceTask :: Task -> Task -> [Task] -> [Task]
replaceTask _ _ [] = []
replaceTask old new (t:ts)
  | old == t  = new:ts
  | otherwise = t:(replaceTask old new ts)

testTs :: [Task]
testTs = [Task False "task1" "desc1", Task True "task2" "desc2", Task False "" ""]

testT :: Task
testT = head testTs


