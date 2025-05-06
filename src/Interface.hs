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
  liftIO $ putStr "Enter command: " -- liftIO $ hFlush stdout <<-- not sure I need it
  input <- liftIO $ getLine 
  state <- get
  liftIO $ printBoard $ board $ game state
  printLn input
  -- if isLooping
  --   then loop
  --   else return ()

handleInput :: String -> StateT AppState IO ()
handleInput "exit" = do
  liftIO $ putStrLn "Goodbye!"
handleInput "standard" = do
  liftIO $ putStrLn "Enter board size:"
  liftIO $ putStrLn "Enter winning streak amount:"
  liftIO $ putStrLn "Enter game type:"
  liftIO $ putStrLn "1. Classic"
  liftIO $ putStrLn "2. Disappearing"
  case mkGame 3 3 of
    Nothing -> liftIO $ putStrLn "err"
    Just game -> modify (\s -> s {game=game, isLooping=False})
handleInput input = do
  liftIO $ putStrLn $ "You entered: " ++ input

-- | helper function to avoid using liftIO everytime I print something inside a StateT function.
printLn :: String -> StateT AppState IO ()
printLn str = do 
  liftIO $ putStrLn str

getLn :: StateT AppState IO String
getLn = do 
  input <- liftIO $ getLine
  return input

