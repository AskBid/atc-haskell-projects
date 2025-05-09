module Main where

import Control.Monad.State

import Interface
import Game

main :: IO ()
main = do
  putStrLn ""
  putStrLn "        X X O O"
  putStrLn "---------O---X---------"
  putStrLn "Welcome to Tic Tac Toe!"
  putStrLn "-----------------------"
  let gm = mkGame 3 3 
  runStateT loop $ AppState gm True 0 0
  putStrLn "------loop end.-------" 
