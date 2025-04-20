module File where

import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)

import Task

saveTasks :: [Task] -> IO ()
saveTasks ts = writeFile "todo.txt" $ concatMap writeTask ts

-- | writeTask is almost as a @show@ function but used to save a Task to file
writeTask :: Task -> String
writeTask t = (show $ completed t) ++
  "," ++ (name t) ++ 
  ",\"" ++ (description t) ++ "\"\n"

readTasks :: IO ([Task])
readTasks = do 
  string <- readFile "todo.txt"
  let tasks = parse (many parserTask) "" string
  case tasks of
    Left err -> do
      putStrLn "Something went wrong while reading todo list from file."
      pure []
    Right ts -> do
      putStrLn "Todo list read from file."
      pure ts

-- probably no need for a parser here as a @split ','@ would be enough..
-- but I was willing to put to practice the Parsec library I just read on.
-- | parserCheck given a string.row from the todo.txt file converts it into Task type.
-- >>> parse parserTask "" "task-name,False,description of the task\n"
parserTask :: Parser Task
parserTask = do
  mark <- parserCompleted
  char ','
  name <- parserTaskName
  char ','
  description <- parserDescription
  char '\n'
  notFollowedBy eof --(noneOf "\n") <|> eof
  return (Task mark name description)

-- | parserTaskName checks validity of Task name. 
--   Allows for empty name for now as it comes handy with CLI when nochange is wanted,  
--   should be changed in the future.
parserTaskName :: Parser String
parserTaskName = do
  name <- many $ letter <|> digit <|> oneOf "-. "
  notFollowedBy (noneOf ",") <|> eof
  return name

parserDescription :: Parser String
parserDescription = do
  char '"'
  description <- many (noneOf "\"")
  char '"'-- anyChar
  return description

parserCompleted :: Parser Bool
parserCompleted = do
  mark <- string "True" <|> string "False"
  notFollowedBy (noneOf ",") <|> eof
  return $ read mark
