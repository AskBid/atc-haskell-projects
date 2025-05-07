{-# LANGUAGE RecordWildCards #-}

module Game 
  ( Game(..) 
  , mkGame
  , win
  , winStreak
  , move
  ) where

import Data.Maybe (isNothing, fromMaybe)

import Board
  ( Board(..)
  , Player(..) 
  , Coordinate(..)
  , mkBoard
  , boardlines 
  , placePawn
  )
import Helpers as H

data Game = Game
  { board       :: Board 
  , turn        :: Player
  , countToWin  :: CountToWin
  } deriving Show
type CountToWin = Int

-- | uses placePawn to make a move in game, considering turn.
move :: Coordinate -> Game -> Either String Game
move xy Game{..} = 
  case tryMove of  
    Nothing       -> Left "err: The move is not valid."
    Just newBoard -> Right $ Game newBoard nextPlayer countToWin
  where
    otherPlayer O = X
    otherPlayer X = O
    nextPlayer = otherPlayer turn
    tryMove = placePawn nextPlayer xy board

mkGame :: Int -> CountToWin -> Maybe Game
mkGame boardL ctw 
  | ctw <= boardL && ctw > 1 = Just $ Game (mkBoard boardL) O ctw
  | otherwise                = Nothing

-- | given a @Game@ as argument returns a @Just Player@ if a winner is found,
--   or @Nothing@ if the game isn't finished.
win :: Game -> Maybe Player
win (Game b _ ctw) = fromMaybe Nothing winner 
  where
    notWin p = isNothing p
    winner = H.head (dropWhile notWin $ winStreak ctw <$> (boardlines b))

-- | Find the winner in a line from @Board.boardlines@.
--   @Game@ argument is only used to get the @countToWin@ value that gives the
--   lenght of tokeens required for a win. 
--   @[Maybe Player]@ is the line being analysed to find a winner.
winStreak :: CountToWin -> [Maybe Player] -> Maybe Player
winStreak _ [] = Nothing
winStreak ctw (p:ps)
  | pawns maxStreak >= ctw = player maxStreak 
  | otherwise              = Nothing
  where
    maxStreak = foldl compare (StreakCount 1 p) ps
    compare (StreakCount c p) p'
      | c >= ctw     = StreakCount c p
      | isNothing p' = StreakCount 1 p'
      | p' == p      = StreakCount (c+1) p'
      | otherwise    = StreakCount 1 p'

data StreakCount = StreakCount
  { pawns  :: Int
  , player :: Maybe Player
  }
