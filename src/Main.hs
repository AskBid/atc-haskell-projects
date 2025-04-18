module Main where

import Task
import Interface

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  loop testTs


