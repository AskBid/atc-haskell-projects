module Interface where

import System.IO (hFlush, stdout)

import Task
import File

loop :: [Task] -> IO ()
loop ts = do
  -- read file
  putStr "Enter command: "
  hFlush stdout -- insures output is printed immediately rather than wait in buffer
  input <- getLine
  (isLooping, tsNew) <- handleInput ts input
  if isLooping
    then loop tsNew
    else return ()

-- | handleInput to dispatch the input command from CLI interface.
--   @etask@ stands for eitherTask, as all modification to the TodoList
--   try to find the Task first and later replace it with a modified Task.
handleInput :: [Task] -> String -> IO (Bool, [Task])
handleInput ts "exit" = do
  -- write
  putStrLn "Goodbye!"
  pure (False, ts)

handleInput ts "add" = do
  putStrLn "Enter task:"
  name <- getLine
  pure (True, Task False name "desc":ts)

handleInput ts "view" = do
  sequenceA $ (putStrLn.show) <$> ts 
  pure (True, ts)

handleInput ts "mark" = do
  putStrLn "Enter completed task:"
  nameInput <- getLine
  let taskSearch = Task False nameInput ""
  let etask = getTask (\t -> t == taskSearch) ts
  case etask of
    Left err   -> do
      putStrLn err
      putStrLn "(or the task was already completed)"
      pure (True, ts)
    Right task -> do
      let markTask = Task True (name task) (description task)
      let newTS = replaceTask task markTask ts 
      putStrLn $ (name task) ++ "task marked as completed."
      pure (True, newTS)

handleInput ts "delete" = do
  putStrLn "Enter task to delete:"
  nameInput <- getLine
  let etask = getTask (\t -> (name t) == nameInput) ts
  case etask of
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
  let etask = getTask (\t -> (name t) == nameInput) ts
  case etask of
    Left err   -> do
      putStrLn err
      pure (True, ts)
    Right task -> do
      putStrLn "Current task description:"
      putStrLn $ description task
      putStrLn "Enter new name or leave empty to keep the same:"
      nameIO <- getLine
      let newName = if nameIO == "" then name task else nameIO
      putStrLn "Enter new description or leave empty to keep the same:"
      descriptionIO <- getLine
      let newDescription = if descriptionIO == "" then description task else descriptionIO
      let newTask = Task (completed task) newName newDescription
      let newTS = replaceTask task newTask ts
      putStrLn "Task edited:"
      putStrLn $ show newTask
      pure (True, newTS)

handleInput ts input = do
  putStrLn $ "You entered: " ++ input
  putStrLn "Not a command."
  pure (True, ts)




