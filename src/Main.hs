{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Text.Parsec (parse)

import File (readTasks)
import Interface (loop, interfaceHelp)
import Task (sortTasks, mkTask)
import Parser (parserTaskName, nameParserError)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> interface
    ("add":name:_) -> add name
    ("--help":_) -> help
    ("-h":_) -> help
    otherwise -> putStrLn "Unknown command." 

interface :: IO ()
interface = do
  putStrLn "Welcome to my TODO List Manager!"
  ts <- readTasks
  loop $ sortTasks ts

add :: String -> IO ()
add name = do 
  case parse parserTaskName "" name of
    Left _ -> putStrLn nameParserError 
    Right name -> do
      task <- mkTask name
      ts <- readTasks
      pure $ sortTasks $ task:ts
      putStrLn $ "\" " ++ name ++ "\" task was written in the database."

help :: IO ()
help = do
  let textBlock = unlines [ "Usage:"
                          , "  todo [<OPTIONS>]           Starts the CLI interactive interface" 
                          , "  todo add <TASK_NAME>       Quickly adds a task to the locally saved todolist"
                          , "  todo view                  Shows a list of completed and uncompleted tasks"
                          , ""
                          , "Available options:"
                          , "  --help, -h                 Shows this help text"
                          , ""]
  putStrLn textBlock
  interfaceHelp
