module Interface where

import System.IO (hFlush, stdout)
import Control.Monad.State
import Control.Monad (when)
-- import Control.Monad.IO.Class
import Data.Maybe (isNothing)
import Text.Parsec (parse)

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
--
handleInput "0" = do
  modify (\s -> s {playing = True})
  printLn "Quick Standard Game Started!"
  gameLoop
-- 
handleInput "2" = do
  state <- get
  displayGame state
  printLn "Enter board size:"
  printLn "Enter winning streak amount:"
-- 
handleInput "help" = do
  printLn "exit"
-- 
handleInput input = do
  printLn $ "You entered: " ++ input
  printLn "Not valid entry."


setupGame :: StateT AppState IO ()
setupGame = undefined

gameLoop :: StateT AppState IO ()
gameLoop = do
  state <- get
  displayGame state 
  let player = otherPlayer $ lastMove $ game state
  printLn $ "Enter coordinates for player `" ++ show player ++ "` next move"
  input <- getLn
  case parse (coordinateParser ["A","B","C"]) "" input of
    Left e   -> printLn "err: Bad coordinate input."
    Right xy -> case move xy $ game state of
      Left e   -> printLn e
      Right gm -> modify (\s -> s {game=gm})
  state' <- get
  when (isNothing $ win $ game state') gameLoop

menu :: StateT AppState IO ()
menu = do 
  printLn ""
  printLn "0. Quick Standard 3x3 Game"
  printLn "1. Quick Evaporating 3x3 Game"
  printLn "2. Stadard Style Game Setup"
  printLn "3. Evaporating Style Game Setup"
  printLn ""
  printLn "Select a number."

displayGame :: AppState -> StateT AppState IO ()
displayGame state = do
  liftIO $ printBoard $ board $ game state

-- | helper function to avoid using liftIO everytime I print something inside a StateT function.
printLn :: String -> StateT AppState IO ()
printLn str = do 
  liftIO $ putStrLn str

getLn :: StateT AppState IO String
getLn = do 
  input <- liftIO $ getLine
  return input

