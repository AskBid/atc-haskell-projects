module Interface where

import System.IO (hFlush, stdout)

import Task

loop :: [Task] -> IO ()
loop ts = do
  putStr "Enter command: "
  hFlush stdout -- insures output is printed immediately rather than wait in buffer
  input <- getLine
  (isLooping, tsNew) <- handleInput ts input
  if isLooping
    then loop tsNew
    else return ()

handleInput :: [Task] -> String -> IO (Bool, [Task])
handleInput ts "exit" = do
  --write
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
  let etask = getTask (Task False nameInput "") ts
  case etask of
    Left err           -> do
      putStrLn err
      putStrLn "(or the task was already completed)"
      pure (True, ts)
    Right task -> do
      let newTS = replaceTask task (Task True (name task) (description task)) ts 
      putStrLn $ (name task) ++ "task marked as completed."
      pure (True, newTS)

handleInput ts "delete" = do
  undefined
  pure (True, ts)

handleInput ts "edit" = do
  undefined
  pure (True, ts)

handleInput ts input = do
  putStrLn $ "You entered: " ++ input
  pure (True, ts)




