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
  putStrLn "Goodbye!"
  pure (False, ts)
handleInput ts "add" = do
  putStrLn "Enter task:"
  name <- getLine
  pure (True, Task False name:ts)
handleInput ts "view" = do
  sequenceA $ (putStrLn.show) <$> ts 
  pure (True, ts)
handleInput ts input = do
  putStrLn $ "You entered: " ++ input
  pure (True, ts)

data Task = Task
  { completed :: Bool
  , name      :: String
  } deriving Show
