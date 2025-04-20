module Main where

import File
import Interface

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  ts <- readTasks
  putStrLn $ "miaooo " ++ (show ts)
  loop ts


