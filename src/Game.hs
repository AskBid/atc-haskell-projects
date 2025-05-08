{-# LANGUAGE RecordWildCards #-}

module Game 
  ( Game(..) 
  , mkGame
  , win
  , end
  , winStreak
  , move
  , otherPlayer
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
  , lastPlayer  :: Player
  , countToWin  :: CountToWin
  , disappearingCount :: Maybe Int
  , movesHistory :: Moves
  } deriving Show
type CountToWin = Int

data Moves = Moves {playerO :: [(Int,Int)], playerX :: [(Int,Int)]} 
  deriving Show

-- | uses placePawn to make a move in game, considering turn.
move :: Coordinate -> Game -> Either String Game
move xy Game{..} = 
  case tryMove of  
    Nothing       -> Left "err: The move is not valid."
    Just newBoard -> do 
      let gm = Game newBoard movePlayer countToWin disappearingCount movesHistory
      if isNothing disappearingCount
      then Right $ gm 
      else Right $ disappearingMove xy gm
  where
    movePlayer = otherPlayer lastPlayer
    tryMove = placePawn movePlayer xy board

disappearingMove :: Coordinate -> Game -> Game
disappearingMove Coordinate{..} Game{..}
  | length mvs < countToWin = undefined -- add new move 
  | otherwise = undefined               -- remove oldest move, delete pawn from board, add new move,
  where 
    mvs = getMoves lastPlayer movesHistory

getMoves :: Player -> Moves -> [(Int,Int)]
getMoves O Moves{..} = playerO
getMoves X Moves{..} = playerX
    
otherPlayer :: Player -> Player
otherPlayer O = X
otherPlayer X = O

mkGame :: Int -> CountToWin -> Game
mkGame boardL ctw = Game (mkBoard boardL) X ctw Nothing $ Moves [] []

-- | asses if a board doesn't have any empty spaces.
end :: Game -> Bool
end Game{..} =  0 == (length $ filter isNothing flatBoard)
  where
    flatBoard = concat board

-- | given a @Game@ as argument returns a @Just Player@ if a winner is found,
--   or @Nothing@ if the game isn't finished.
win :: Game -> Maybe Player
win (Game b _ ctw _ _) = fromMaybe Nothing winner 
  where
    notWin p = isNothing p
    winner = H.head (dropWhile notWin $ winStreak ctw <$> (boardlines b))

-- | Find the winner in a line from @Board.boardlines@.
--   from @Game@ we get the @countToWin@ value that gives the
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
