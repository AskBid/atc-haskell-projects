module Main where

import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  loop []

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
  name <- getLine
  task <- getTask name ts
  -- mark completed
  -- replace in [Task]
  pure (True, ts)
handleInput ts "delete" = do
  undefined
  pure (True, ts)
handleInput ts "edit" = do
  undefined
  pure (True, ts)
handleInput ts input = do
  putStrLn $ "You entered: " ++ input
  pure (True, ts)

data Task = Task
  { completed :: Bool
  , name      :: String
  , description :: String
  } deriving Show

getTask :: String -> [Task] -> Maybe Task
-- getTask _ [] = Nothing
getTask name ts = case (filter (\(Task _ name' _) -> name' == name) ts) of  
  []    -> Nothing
  (t:_) -> Just t
