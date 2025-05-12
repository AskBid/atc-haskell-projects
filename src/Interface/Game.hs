{-# LANGUAGE OverloadedStrings #-}
-- | In this module all the CLI oprations to handle actions during the game cycle.
module Interface.Game 
  ( gameLoop
  , endGame
  ) where

import Control.Monad.State
  ( MonadTrans(lift)
  , MonadState(get)
  , StateT
  , modify
  )
import Control.Monad (when)
import Data.Maybe (isNothing, fromMaybe)
import Text.Parsec (parse)
import Control.Monad.State (liftIO)

import Board (Coordinate(..), Player(..), mkBoard) 
import Game (Game(..), move, end, win, otherPlayer)
import Parser (coordinateParser)
import Graphics (charsIxs, charIndexes8)
import Ai (findAiMove)
import Interface.Helpers (displayGame, printLn, getLn)
import Interface.Settings (switchStartingPlayer)
import Interface.AppState


-- | the part of CLI where a recursive function get one move after the other,
--   switching players
gameLoop :: StateT AppState IO ()
gameLoop = do
  state <- get
  displayGame state 
  let player = otherPlayer $ lastPlayer $ game state
  if takePlayersStatus player state 
    then getAiCoordinates state
    else getPlayerCoordinates player state
  state' <- get
  let isWin = not $ isNothing $ win $ game state'
  when (not isWin && (not $ end $ game state')) gameLoop
  when (isWin || (end $ game state')) switchStartingPlayer
    where 
      takePlayersStatus X state = aiX state 
      takePlayersStatus O state = aiO state

-- | the part of CLI taking the coordinate from the human players. 
--   the first argument @Player@ is only used to display information and not involved in 
--   state modification.
getPlayerCoordinates :: Player -> AppState -> StateT AppState IO ()
getPlayerCoordinates p state = do 
  printLn $ "Enter coordinates for player `" ++ show p ++ "` next move. (e.g. a1, A1, a 1, 1 1)"
  input <- getLn
  let letterIndexes = take (length $ board $ game state) $ charIndexes8 charsIxs 
  case parse (coordinateParser letterIndexes) "" input of
    Left e   -> printLn "You entered an invlaid coordinate format,\n\
                        \please stick to this patter examples:\n\
                        \`a1`, `A1`, `a 1`, `A 1`, `1 1`."
    Right xy -> moveInGameState xy $ game state

-- | the part of CLI taking the coordinate from the Ai module.
getAiCoordinates :: AppState -> StateT AppState IO ()
getAiCoordinates state = do 
  let gm = game state
  coords <- lift $ findAiMove gm
  moveInGameState coords gm

-- | makes the move once taken coordinates from Player or Ai inputs.
moveInGameState :: Coordinate -> Game -> StateT AppState IO ()
moveInGameState xy gm = do 
  case move xy gm of
    Left e   -> printLn e
    Right gm -> modify (\s -> s {game=gm})

-- | clears board and updates scores. Change of player turn is handles in @Game.move@.
endGame :: StateT AppState IO ()
endGame = do
  state <- get
  displayGame state
  let winner = win $ game state 
  if isNothing winner 
    then printLn "The game was a Draw!"
    else do
      modify $ dispatchScores (fromMaybe X winner)
      printLn   "* * * * * * *"
      printLn $ "Winner is: " ++ (show $ fromMaybe O winner)
      printLn   "* * * * * * *"
  clearBoard

-- | replaces esxisting board with a new board of Nothings, 
--   based on length of exisiting baord.
clearBoard :: StateT AppState IO ()
clearBoard = do
  state <- get
  let gm = game state
  let boardSize = length $ board gm
  modify (\s -> s {game= gm {board= mkBoard boardSize}})

-- | helps updating scores after each game (@endGame@).
dispatchScores :: Player -> AppState -> AppState
dispatchScores p as 
  | p == O = as{scoresO= pOs}
  | p == X = as{scoresX= pXs}
  where
    ctw = countToWin $ game as
    pOs = scoresO as + ctw
    pXs = scoresX as + ctw
