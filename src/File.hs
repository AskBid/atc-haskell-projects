module File where

import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)

import Task

saveTasks :: [Task] -> IO ()
saveTasks ts = writeFile "todo.txt" $ concatMap writeTask ts

writeTask :: Task -> String
writeTask t = (name t) ++
  "," ++ (show $ completed t) ++ 
  "," ++ (description t) ++ "\n"

parserTask :: Parser Task
parserTask = do
  name <- many letter
  char ','
  description <- many letter
  return (Task False name description)
