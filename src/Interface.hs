module Interface where

import System.IO (hFlush, stdout)
import Control.Monad.State
import Control.Monad (when)
-- import Control.Monad.IO.Class
-- import Data.Maybe

import Game
import Board
import Graphics
import Parser

data AppState = AppState
  { game :: Game
  , isLooping :: Bool
  , playing :: Bool
  } -- deriving (Show)

loop :: StateT AppState IO ()
loop = do
  state <- get
  when (not $ playing state) menu  
  printLn "Enter command/selection: " -- liftIO $ hFlush stdout -- not sure I need it
  input <- getLn 
  handleInput input
  state <- get
  when (isLooping state) loop

handleInput :: String -> StateT AppState IO ()
handleInput "exit" = do
  printLn "Goodbye!"
  modify (\s -> s {isLooping = False})
handleInput "0" = do
  printLn ""
  input <- getLn
  modify (\s -> s {playing = True})
  handleGame input
handleInput "2" = do
  state <- get
  displayGame
  printLn "Enter board size:"
  printLn "Enter winning streak amount:"
  modify (\s -> s {playing = True})
handleInput "help" = do
  printLn "exit"
handleInput input = do
  printLn $ "You entered: " ++ input
  printLn "Not valid entry."

handleGame :: String -> StateT AppState IO ()
handleGame = undefined

menu :: StateT AppState IO ()
menu = do 
  printLn ""
  printLn "0. Quick Standard 3x3 Game"
  printLn "1. Quick Evaporating 3x3 Game"
  printLn "2. Stadard Style Game Setup"
  printLn "3. Evaporating Style Game Setup"
  printLn ""
  printLn "Select a number."

displayGame :: StateT AppState IO ()
displayGame = do
  state <- get
  liftIO $ printBoard $ board $ game state

-- | helper function to avoid using liftIO everytime I print something inside a StateT function.
printLn :: String -> StateT AppState IO ()
printLn str = do 
  liftIO $ putStrLn str

getLn :: StateT AppState IO String
getLn = do 
  input <- liftIO $ getLine
  return input

