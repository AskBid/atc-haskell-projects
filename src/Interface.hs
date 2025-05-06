module Interface where

import System.IO (hFlush, stdout)
import Control.Monad.State
-- import Control.Monad.IO.Class
-- import Data.Maybe

import Game
import Board
import Graphics

data AppState = AppState
  { game :: Game
  , isLooping :: Bool
  } -- deriving (Show)

loop :: StateT AppState IO ()
loop = do
  printLn "Enter command: " -- liftIO $ hFlush stdout <<-- not sure I need it
  input <- liftIO $ getLine 
  state <- get
  liftIO $ printBoard $ board $ game state
  printLn input
  -- if isLooping
  --   then loop
  --   else return ()

handleInput :: String -> StateT AppState IO ()
handleInput "exit" = do
  printLn "Goodbye!"
handleInput "standard" = do
  input <- getLn
  printLn "Enter board size:"
  printLn "Enter winning streak amount:"
  printLn "Enter game type:"
  printLn "1. Classic"
  printLn "2. Disappearing"
handleInput input = do
  printLn $ "You entered: " ++ input

-- | helper function to avoid using liftIO everytime I print something inside a StateT function.
printLn :: String -> StateT AppState IO ()
printLn str = do 
  liftIO $ putStrLn str

getLn :: StateT AppState IO String
getLn = do 
  input <- liftIO $ getLine
  return input

