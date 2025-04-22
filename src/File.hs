{-# LANGUAGE OverloadedStrings #-}

module File (saveTasks, readTasks) where

import System.IO
import Text.Parsec
import Text.Parsec.String

import Parser
import Task

saveTasks :: [Task] -> IO ()
saveTasks ts = writeFile "todo.txt" $ concatMap writeTask ts

-- | writeTask is almost as a @show@ function but used to save a Task to file
writeTask :: Task -> String
writeTask t = (show $ completed t) ++
  "," ++ (name t) ++ 
  "," ++ (show $ priority t) ++ 
  "," ++ (show $ date t) ++ 
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
