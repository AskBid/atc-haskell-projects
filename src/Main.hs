{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment

import File (readTasks)
import Interface (loop)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show args
  putStrLn "Welcome to my TODO List Manager!"
  ts <- readTasks
  loop ts
