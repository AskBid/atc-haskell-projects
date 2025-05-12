{-# LANGUAGE OverloadedStrings #-}

module Interface.Helpers where

import Control.Monad.State
  ( MonadIO(liftIO)
  , MonadState(get)
  , StateT
  , modify
  )

import Game (Game(lastPlayer, board), otherPlayer)
import Graphics (printBoard)
import Interface.AppState

-- | @gameLoop@ always set the current player as the opposite to the last move (@lastPlayer@).
--   this function makes sure that at the beginning of each game the initial player (in 
--   AppState) has effect, changing the @lastPlayer@ state value accordingly.
setStartintPawn :: StateT AppState IO ()
setStartintPawn = do
  state <- get
  let gm = game state
  let lastP = otherPlayer $ startingPawn state
  let gm' = gm{lastPlayer= lastP}
  modify (\s -> s{game= gm'})

-- | print board on screen with indexes.
displayGame :: AppState -> StateT AppState IO ()
displayGame state = do
  printLn "\\/"
  printLn ""
  liftIO $ printBoard $ board $ game state
  printLn ""
  printLn ""

-- | helper function to avoid using liftIO everytime I print something inside a StateT function.
printLn :: String -> StateT AppState IO ()
printLn str = do 
  liftIO $ putStrLn str

getLn :: StateT AppState IO String
getLn = do 
  input <- liftIO $ getLine
  return input
