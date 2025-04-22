{-# LANGUAGE OverloadedStrings #-}

module Interface where

import System.IO (hFlush, stdout)
import Text.Parsec (parse)
import Data.Time (getCurrentTime, utctDay, Day)

import Task
import File
import Parser

loop :: [Task] -> IO ()
loop ts = do
  putStr "Enter command: "
  hFlush stdout -- insures output is printed immediately rather than wait in buffer
  input <- getLine
  (isLooping, tsNew) <- handleInput ts input
  if isLooping
    then loop tsNew
    else return ()

-- | handleInput to dispatch the input command from CLI interface.
--   all modification to the TodoList try to find the Task first and 
--   later replace it with a modified Task.
handleInput :: [Task] -> String -> IO (Bool, [Task])
handleInput ts "exit" = do
  saveTasks ts
  putStrLn "Goodbye!"
  pure (False, [])

handleInput ts "add" = do
  putStrLn "Enter task:"
  name <- nameCheck
  task <- mkTask name
  task'<- enterTaskAttributes task
  pure (True, task:ts)

handleInput ts "view" = do
  sequenceA $ (putStrLn.show) <$> ts 
  pure (True, ts)

handleInput ts "mark" = do
  putStrLn "Enter completed task:"
  nameInput <- getLine
  taskSearch <- mkTask nameInput
  case getTask (\t -> t == taskSearch) ts of
    Left err   -> do
      putStrLn err
      putStrLn "(or the task was already completed)"
      pure (True, ts)
    Right task -> do
      let markTask = Task True (name task) (priority task) (date task) (description task)
      let newTS = replaceTask task markTask ts 
      putStrLn $ (name task) ++ " task marked as completed."
      pure (True, newTS)

handleInput ts "delete" = do
  putStrLn "Enter task to delete:"
  nameInput <- getLine
  case getTask (\t -> (name t) == nameInput) ts of
    Left err   -> do
      putStrLn err
      putStrLn "You can't delete a task that doesn't exist"
      pure (True, ts)
    Right task -> do
      let newTS = deleteTask task ts 
      putStrLn $ nameInput ++ " task was deleted."
      pure (True, newTS)

handleInput ts "edit" = do
  putStrLn "Enter task to edit:"
  nameInput <- getLine
  case getTask (\t -> (name t) == nameInput) ts of
    Left err   -> do
      putStrLn err
      pure (True, ts)
    Right task -> do 
      newTask <- enterTaskAttributes task
      let newTS = replaceTask task newTask ts
      pure (True, newTS)

handleInput ts input = do
  putStrLn $ "You entered: " ++ input
  putStrLn "Not a command."
  pure (True, ts)

-- | enterTaskAttributes is the part of the CLI interface used to deal with 
--   @Task@ attributes insertion or edits.
enterTaskAttributes :: Task -> IO Task
enterTaskAttributes task = do  
  putStrLn $ "Current due date: " 
  putStrLn $ show $ date task
  putStrLn "Enter due date (YYYY-MM-DD) or number of days for duration, leave empty to leave unchanghed"
  putStrLn "Current task description:"
  putStrLn $ "\"" ++ description task ++ "\""
  putStrLn "Enter new name or leave empty to keep the same:"
  nameIO <- nameCheck
  let newName = 
        if nameIO == "" 
        then name task 
        else nameIO
  putStrLn "Enter new description or leave empty to keep the same:"
  descriptionIO <- getLine
  let newDescription = 
        if descriptionIO == "" 
        then description task 
        else descriptionIO
  let newTask = Task (completed task) newName (priority task) (date task) newDescription
  putStrLn "Task edited/created:"
  putStrLn $ show newTask
  return newTask

-- | nameCheck uses the Parser to read from file "todo.txt" for checking CLI inputs have 
--   a compatible format. It does loop if no copatible input is given.
nameCheck :: IO (String)
nameCheck = do
  name <- getLine
  let check = parse parserTaskName "" name
  case check of 
    Left e -> do
      putStrLn "Only letters, `-`, `.` and spaces are accepted for names."
      putStrLn "Enter name again:"
      nameCheck
    Right n -> pure n

-- | processCurrentDay is used as argument for the @Parser.parserDueDate@ to get
--   today's date in Day type format, so that we can enter a nuber of days to set
--   the due date from today's date.
processCurrentDay :: IO Day
processCurrentDay = do
  currentTime <- getCurrentTime
  return $ utctDay currentTime


