{-# LANGUAGE OverloadedStrings #-}

module Interface (multiplayer, loop) where

import System.IO (hFlush, stdout)
import Control.Monad.State
import Control.Monad (when)
-- import Control.Monad.IO.Class
import Data.Maybe (isNothing, fromMaybe)
import Text.Parsec (parse)

import Game (Game(..), mkGame)
import Board (Player(O,X))
import Interface.Game (gameLoop, endGame)
import Interface.Helpers (setStartintPawn, printLn, getLn)
import Interface.Settings (switchStartingPlayer, buildCustomGame)
import Interface.AppState

-- | dialogue to set players as human or AI, used at the very beginning of the CLI.
multiplayer :: AppState -> IO AppState
multiplayer as = do
  as' <- interface X as
  interface O as' 
  where
    interface p as = do
      putStrLn $ "Player `" ++ show p ++ "` should be AI or Human?:"
      putStrLn "(`ai`, `human`, press enter for default (AI)"
      input <- getLine
      case parseAiInput input of
        Just bool -> dispatchChange bool p as
        Nothing   -> do 
          putStrLn "please enter only `human` or `ai`"
          interface p as
    dispatchChange b p as 
      | p == X = return as{aiX= b}
      | p == O = return as{aiO= b}
    parseAiInput ""      = Just True 
    parseAiInput "ai"    = Just True 
    parseAiInput "human" = Just False 
    parseAiInput otherwise = Nothing

-- | acts as main menu.
loop :: StateT AppState IO ()
loop = do
  state <- get
  printLn "-------------------------------------------------------------------\n\ 
          \COMMAND   |                                             DESCRIPTION\n\
          \----------|--------------------------------------------------------\n\
          \0         |                                 Quick Standard 3x3 Game\n\
          \1         |                              Quick Evaporating 3x3 Game\n\
          \2         |                   Stadard/Evaporating Custom Game Setup\n\
          \3         |                                        repeat last game\n\
          \          |                                                        \n\
          \repeat    |                                        repeat last game\n\
          \scores    |                  Shows the scores of all games this far\n\
          \starting  |                     Switch the player that starts first\n\
          \reset     |                  Reinitiate App (select Multiplayer/AI)\n\
          \exit      |                                       Exit from the App\n\
          \-------------------------------------------------------------------"
  printLn $ "Player to start next: " ++ (show $ startingPawn state)
  printLn ""
  printLn "Please, enter a command from the list above:"
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
  setStartintPawn
  gameLoop
  endGame 
-- 
handleInput "1" = do
  printLn "Quick Evaporating Game Started!"
  modify (\s -> s{game=(game s){evaporCount= Just 3}})
  setStartintPawn
  gameLoop
  endGame 
-- 
handleInput "2" = do
  printLn ""
  buildCustomGame
  setStartintPawn
  gameLoop
  endGame
--
handleInput "3" = do
  state <- get
  printLn ""
  printLn "Replaying game with:"
  printLn $ "Evaporating value: " ++ (show $ evaporCount $ game state)
  printLn $ "Winning strake value: " ++ (show $ countToWin $ game state)
  setStartintPawn
  gameLoop
  endGame
--
handleInput "repeat" = handleInput "3"
--  
handleInput "starting" = do
  switchStartingPlayer
-- 
handleInput "scores" = do
  state <- get
  printLn ""
  printLn "Scores are calculated by assigning the winning streak for each win."
  printLn $ "Player X scores: " ++ (show $ scoresX state) 
  printLn $ "Player O scores: " ++ (show $ scoresO state) 
  printLn ""
  printLn "press Enter to continue."
  input <- getLn
  printLn ""
-- 
handleInput "reset" = do
  let app = AppState (mkGame 3 3) True 0 0 True True X
  printLn "Games history reset."
  app' <- liftIO $ multiplayer app
  modify $ const app'
-- 
handleInput input = do
  printLn $ "You entered: " ++ input
  printLn $ "err: `" ++ input ++ "` is not a valid command."

