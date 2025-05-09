module Main where

import Control.Monad.State

import Interface
import Game
import Board (Player(..))

main :: IO ()
main = do
  putStrLn ""
  putStrLn "        X X O O"
  putStrLn "---------O---X---------"
  putStrLn "Welcome to Tic Tac Toe!"
  putStrLn "-----------------------"
  let gm = mkGame 3 3 
  let app = AppState gm True 0 0 True X
  app' <- multiplayer app
  runStateT loop app' 
  putStrLn "------loop end.-------" 
