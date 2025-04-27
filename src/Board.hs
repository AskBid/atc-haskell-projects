module Board where

import Data.List (transpose)
import Data.Maybe (isNothing)

data Player = O | X 
  deriving (Show, Eq)

type Board = [[Maybe Player]]

data Game = Game
  { board       :: Board 
  , turn        :: Player
  , countToWin  :: Int
  }

mkGame :: Int -> Int -> Maybe Game
mkGame boardL ctw 
  | ctw <= boardL && ctw > 1 = Just $ Game (mkBoard boardL) O ctw
  | otherwise                = Nothing

mkBoard :: Int -> Board
mkBoard n = take n $ repeat (take n $ repeat Nothing)

-- |
-- Qs: is using Game bad for performance? trade off with clarity/solidity.
winStreak :: Game -> [Maybe Player] -> Maybe Player
winStreak _ [] = Nothing
winStreak game (p:ps) = if fst maxStreak >= ctw 
                        then snd maxStreak 
                        else Nothing
  where
    ctw = countToWin game
    maxStreak = foldl compare (1, p) ps
    compare (count, p) p'
      | count >= ctw = (count, p)
      | isNothing p' = (1, p')
      | p' == p      = (count+1, p')
      | otherwise    = (1, p')

boardlines :: Board -> [[Maybe Player]]
boardlines rows = rows ++ columns ++ diagonals
  where
    columns = transpose rows
    len = length rows - 1
    diagonals = transpose [[rows !! r !! r, rows !! r !! (len - r)] | r <- rowIxs]
    rowIxs = [0..len]
    colIxs = reverse rowIxs


bb2 = [[Just X, Just O, Nothing]
      ,[Just X, Nothing, Just O]
      ,[Just O, Just X, Nothing]]

bb3 = [[Just X, Just O, Nothing, Just X]
      ,[Just X, Nothing, Just O, Nothing]
      ,[Just O, Just X,  Just X, Just O]
      ,[Just O, Just X, Nothing, Just O]]
