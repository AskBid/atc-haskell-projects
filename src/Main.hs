module Main where

import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  loop

loop :: IO ()
loop = do
  putStr "Enter command: "
  hFlush stdout
  input <- getLine
  isLooping <- handleInput input
  if isLooping
    then loop
    else return ()

handleInput :: String -> IO Bool
handleInput "exit" = do
  putStrLn "Goodbye!"
  pure False
handleInput "add" = do
  putStrLn "Enter task:"
  task <- getLine
  pure True
handleInput input = do
  putStrLn $ "You entered: " ++ input
  pure True

data Task = Task
  { completed :: Bool
  , name      :: String
  } deriving Show
