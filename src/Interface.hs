{-# LANGUAGE OverloadedStrings #-}

module Interface where

import System.IO (hFlush, stdout)
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Data.Time (getCurrentTime, utctDay, Day, diffDays)
import Data.Maybe (fromMaybe)

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

handleInput ts "help" = do
  interfaceHelp
  pure (True, ts)

handleInput ts "add" = do 
  task <- mkTask "*unnamed*"
  task'<- enterTaskAttributes task
  pure (True, sortTasks $ task':ts)

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
      pure (True, sortTasks newTS)

handleInput ts input = do
  putStrLn $ "You entered: " ++ input
  putStrLn "Not a command."
  pure (True, ts)

-- | enterTaskAttributes is the part of the CLI interface used to deal with 
--   @Task@ attributes insertion or edits. It goes through every attribute one
--   by one with the interface. Only completed is left out as that is dealt with
--   the `mark` interface action.
--   It always takes a Task even when newly created.
enterTaskAttributes :: Task -> IO Task
enterTaskAttributes task = do 
  putStrLn ":::"
  putStrLn $ "Current task name: " ++ (name task) ++ " <---" 
  putStrLn "Enter new name or leave empty to keep it the same:"
  nameIO <- entryAndCheck parserTaskName $ "error: " ++ nameParserError
  let newName = fromMaybe (name task) nameIO
  --
  -- Enter or edit date.
  today <- processCurrentDay
  putStrLn ":::"
  let taskDate = date task
  let dateStr = show taskDate ++ " (" ++ (show $ diffDays taskDate today) ++ " days left)"
  putStrLn $ "Current due date: " ++ dateStr
  putStrLn "Enter due date (YYYY-MM-DD) or number of days from today" 
  putStrLn "(press Enter to leave unchanghed):"
  dateIO <- dateEntryAndCheck today
  let newDate = fromMaybe taskDate dateIO
  --
  -- Enter or Edit priority
  putStrLn ":::"
  putStrLn $ "Current task priority: " ++ (show $ priority task) 
  putStrLn "Enter new priority or leave empty to keep the same:"
  priorityIO <- entryAndCheck parserPriority "error: You entered an invalid priority text."
  let newPriority = fromMaybe (priority task) priorityIO
  --
  -- Enter or edit description.
  putStrLn ":::"
  putStrLn "Current task description:"
  putStrLn $ "\"" ++ description task ++ "\""
  putStrLn "Enter new description or leave empty to keep the same:"
  descriptionIO <- entryAndCheck parserDescription "error: You entered an invalid text as description."
  let newDescription = fromMaybe (description task) descriptionIO
  --
  -- build Task and return
  putStrLn ":::"
  let newTask = Task (completed task) newName newPriority newDate newDescription
  putStrLn "Task edited/created:"
  putStrLn $ show newTask
  return newTask

-- | uses the Parser to check that CLI inputs have a compatible format with the 
--   text file parsers. It does loop if no copatible input is given.
entryAndCheck :: Parser a -> String -> IO (Maybe a)
entryAndCheck p myErr = do
  entry <- getLine
  if entry == ""
  then pure Nothing 
  else case parse p "" entry of 
    Left e -> do
      putStrLn myErr 
      putStrLn "Enter again:"
      entryAndCheck p myErr
    Right a -> pure (Just a)

dateEntryAndCheck :: Day -> IO (Maybe Day)
dateEntryAndCheck today = do 
  dateIO <- getLine
  if dateIO == "" 
  then pure Nothing
  else case parse (parserDueDate today) "" dateIO of  
    Left e -> do
      putStrLn "\"error: You entered an invalid date format. Enter (YYYY-MM-DD) or an integer.\""
      putStrLn "Enter date again:"
      dateEntryAndCheck today 
    Right newDate -> pure $ Just newDate

-- | used as argument for the @Parser.parserDueDate@ to get today's date in 
--   Day type format, so that we can enter a nuber of days to set the due date 
--   from today's date.
processCurrentDay :: IO Day
processCurrentDay = do
  currentTime <- getCurrentTime
  return $ utctDay currentTime

interfaceHelp :: IO ()
interfaceHelp = do
  let textBlock = unlines [ "Commands for interactive CLI interface:"
                          , "  exit                       Exits the interactive interface"
                          , "  add                        Dialogue to add a new todo-list-task" 
                          , "  view                       Shows a list of all uncompleted tasks"
                          , "  completed                  Shows a list of all completed tasks"
                          , "  mark                       Dialogue to mark a file as completed"
                          , "  delete                     Dialogue to delete a task"
                          , "  edit                       Dialogue to edit an existing task"
                          , "  help                       Shows the interactive CLI interface available actions" ]
  putStrLn textBlock
