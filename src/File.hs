module File where

import System.IO

import Task

saveTasks :: [Task] -> IO ()
saveTasks ts = writeFile "todo.txt" $ concatMap writeTask ts

writeTask :: Task -> String
writeTask t = (name t) ++
  "," ++ (show $ completed t) ++ 
  "," ++ (description t) ++ "\n"
