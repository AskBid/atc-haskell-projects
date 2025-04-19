module File where

import System.IO

saveTasks :: IO ()
saveTasks = writeFile "todo.txt" "taskname,completed,description,date,priority"
