{-# LANGUAGE OverloadedStrings #-}

module Main where

import File (readTasks)
import Interface (loop)

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  ts <- readTasks
  loop ts
