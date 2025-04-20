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

-- probably no need for a parser here as a @split ','@ would be enough..
-- but I was willing to put to practice the Parsec library I just read on.
parserTask :: Parser Task
parserTask = do
  name <- many letter
  char ','
  description <- many letter
  return (Task False name description)

parserTaskName :: Parser String
parserTaskName = do
  name <- many1 $ letter <|> digit <|> oneOf "-. "
  eof
  return name

parserDescription :: Parser String
parserDescription = do
  description <- many anyChar
  return description

parserCompleted :: Parser Bool
parserCompleted = do
  mark <- string "True" <|> string "False"
  eof
  return $ read mark
