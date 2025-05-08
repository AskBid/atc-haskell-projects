module Interface where

import System.IO (hFlush, stdout)
import Control.Monad.State
import Control.Monad (when)
-- import Control.Monad.IO.Class
import Data.Maybe (isNothing, fromMaybe)
import Text.Parsec (parse)

import Game
import Board
import Graphics
import Parser

data AppState = AppState
  { game :: Game
  , isLooping :: Bool
  } -- deriving (Show)

loop :: StateT AppState IO ()
loop = do
  state <- get
  printLn "------------------------" 
  printLn ""
  printLn "0. Quick Standard 3x3 Game"
  printLn "1. Quick Evaporating 3x3 Game"
  printLn "2. Stadard Style Game Setup"
  printLn "3. Evaporating Style Game Setup"
  printLn ""
  printLn "Select/enter a number or \"help\" to list available commands."
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
  printLn "Quick Standard Game Started!"
  gameLoop
  endGame 
-- 
handleInput "2" = do
  printLn ""
  buildCustomGame
  gameLoop
  endGame
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
  when ((isNothing $ win $ game state') && (not $ end $ game state')) gameLoop

displayGame :: AppState -> StateT AppState IO ()
displayGame state = do
  printLn "\\/"
  printLn ""
  liftIO $ printBoard $ board $ game state
  printLn ""

endGame :: StateT AppState IO ()
endGame = do
  state <- get
  displayGame state
  let winner = win $ game state
  if isNothing winner 
  then printLn "The game was a Draw!"
  else printLn $ "Winner is: " ++ (show $ fromMaybe O winner)
  clearBoard

clearBoard :: StateT AppState IO ()
clearBoard = do
  state <- get
  let gm = game state
  let boardSize = length $ board gm
  modify (\s -> s {game= gm {board= mkBoard boardSize}})

buildCustomGame :: StateT AppState IO ()
buildCustomGame = do 
  boardSize <- liftIO $ getSettingSize "Enter board size:" 100 
  countToWin <- liftIO $ getSettingSize "Enter winning strake count:" boardSize
  modify (\s -> s {game= (mkGame boardSize countToWin)})
  printLn "Custom Game Started!"
  printLn $ "Make a line of "++ show countToWin ++" pawns to win the game!" 

getSettingSize :: String -> Int -> IO Int
getSettingSize str limit = do
  putStrLn str
  input <- getLine 
  case parse (acceptedNumber limit) "" input of
    Left e  -> do 
      putStrLn $ last $ lines $ show e
      getSettingSize str limit
    Right boardSize -> return boardSize 


-- | helper function to avoid using liftIO everytime I print something inside a StateT function.
printLn :: String -> StateT AppState IO ()
printLn str = do 
  liftIO $ putStrLn str

getLn :: StateT AppState IO String
getLn = do 
  input <- liftIO $ getLine
  return input

