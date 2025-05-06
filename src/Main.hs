module Main where

import Control.Monad.State

import Interface
import Game

main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe!"
  let gm = mkGame 3 3
  case gm of
    Nothing -> putStrLn "error"
    Just g -> do 
      runStateT loop $ AppState g True
      putStrLn "loop end." 
