module Interface.AppState where

import Board
import Game

data AppState = AppState
  { game      :: Game
  , isLooping :: Bool
  , scoresO   :: Int
  , scoresX   :: Int
  , aiX       :: Bool
  , aiO       :: Bool
  , startingPawn :: Player
  } deriving (Show)
