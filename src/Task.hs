module Task where

data Task = Task
  { completed :: Bool
  , name      :: String
  , description :: String
  } deriving (Show)

instance Eq Task where
  (Task completed name _) == (Task completed' name' _) = completed == completed' && name' == name

getTask :: Task -> [Task] -> Either String Task
getTask tSearch ts = case filter (\t -> tSearch == t) ts of
  []    -> Left $ "No task \"" ++ (name tSearch) ++ "\" was found. "
  (t:_) -> Right t

replaceTask :: Task -> Task -> [Task] -> [Task]
replaceTask _ _ [] = []
replaceTask old new (t:ts)
  | old == t  = new:ts
  | otherwise = t:(replaceTask old new ts)

testTs :: [Task]
testTs = [Task False "task1" "desc1", Task True "task2" "desc2", Task False "" ""]

testT :: Task
testT = head testTs


