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
  , removePawn
  )
import Helpers as H

data Game = Game
  { board       :: Board 
  , lastPlayer  :: Player
  , countToWin  :: CountToWin
  , evaporCount :: Maybe Int
  , movesHistory :: Moves
  } deriving Show
type CountToWin = Int

data Moves = Moves {playerO :: [Coordinate], playerX :: [Coordinate]} 
  deriving Show

-- | uses placePawn to make a move in game, considering turn.
move :: Coordinate -> Game -> Either String Game
move xy Game{..} = 
  case tryMove of  
    Nothing       -> Left "err: The move is not valid."
    Just newBoard -> do 
      let gm = Game newBoard movePlayer countToWin evaporCount movesHistory
      if isNothing evaporCount
      then Right $ gm 
      else Right $ disappearingMove xy gm
  where
    movePlayer = otherPlayer lastPlayer
    tryMove = placePawn movePlayer xy board

disappearingMove :: Coordinate -> Game -> Game
disappearingMove xy gm
  | length movesP < (fromMaybe (countToWin gm) $ evaporCount gm) = 
      gm {movesHistory= injectMove movesP}
  | otherwise = 
      gm {board= boardNew, movesHistory= injectMove movesP'}
  where 
    movesP = getMoves thisPlayer moves
    thisPlayer = lastPlayer gm
    moves = movesHistory gm
    injectMove ms = insertMovesH thisPlayer (xy:ms) moves
    boardNew = removePawn (last movesP) (board gm)
    movesP' = take ((length movesP)-1) movesP
    insertMovesH :: Player -> [Coordinate] -> Moves -> Moves
    insertMovesH O cs mvs = mvs {playerO= cs}   
    insertMovesH X cs mvs = mvs {playerX= cs}   
    getMoves :: Player -> Moves -> [Coordinate]
    getMoves O Moves{..} = playerO
    getMoves X Moves{..} = playerX

otherPlayer :: Player -> Player
otherPlayer O = X
otherPlayer X = O

-- | notice O as lastPlayer means X will start the game.
mkGame :: Int -> CountToWin -> Game
mkGame boardL ctw = Game (mkBoard boardL) O ctw Nothing $ Moves [] []

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
