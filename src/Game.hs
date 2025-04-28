module Game 
  ( Game(..) 
  , mkGame
  , win
  ) where

import Data.Maybe (isNothing)
import Data.List (uncons)

import Board 
  ( Board(..)
  , Player(..) 
  , mkBoard
  , boardlines 
  )

data Game = Game
  { board       :: Board 
  , turn        :: Player
  , countToWin  :: CountToWin
  }
type CountToWin = Int

mkGame :: Int -> Int -> Maybe Game
mkGame boardL ctw 
  | ctw <= boardL && ctw > 1 = Just $ Game (mkBoard boardL) O ctw
  | otherwise                = Nothing

-- | given a @Game@ as argument returns a Player if a winner is found,
--   or Nothing if the game isn't finished.
win :: Game -> Maybe Player
win (Game b _ ctw) = head' (dropWhile notWin $ winStreak ctw <$> (boardlines b))
  where
    notWin p = isNothing p
    head' []     = Nothing
    head' (p:ps) = p

-- | Find the winner in a line from @Board.boardlines@.
--   @Game@ argument is only used to get the @countToWin@ value that gives the
--   lenght of tokeens required for a win. 
--   @[Maybe Player]@ is the line being analysed to find a winner.
-- Qs: is using Game bad for performance Vs only Int? 
--     trade off with clarity/solidity? // eventually used a type * = Int
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
