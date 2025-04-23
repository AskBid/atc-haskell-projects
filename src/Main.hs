{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Text.Parsec (parse)

import File (readTasks)
import Interface (loop)
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
      putStrLn name

help :: IO ()
help = do
  putStrLn "view"
  putStrLn "add"
  putStrLn "edit"
