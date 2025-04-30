{-# LANGUAGE RecordWildCards #-}

module Board where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Data.List.Index (setAt)

data Player = O | X 
  deriving (Show, Eq)

type Board = [[Maybe Player]]

data Coordinate = Coordinate
  { x :: Int
  , y :: Int }

-- | The minimum number of pawns needed to achieve a winning streak (adjustable).
--   The lower bound for the winning streak pawn count should set at 3.
--   Used to limit the diagonals calculated.
minStreak :: Int
minStreak = 3

-- | like a move function but not including turn check, only valid move check.
placePawn :: Player -> Coordinate -> Board -> Maybe Board
placePawn p Coordinate{..} b 
  | freeSpace (Coordinate x y) b = pure $ setAt y (setAt x (pure p) row) b
  | otherwise = Nothing
  where
    row = b !! x

-- | checks if pawn move is valid.
freeSpace :: Coordinate -> Board -> Bool
freeSpace Coordinate{..} b = isNothing ((b !! y) !! x)

-- | sqaured boards only.
mkBoard :: Int -> Board
mkBoard n = take n $ repeat (take n $ repeat Nothing)

-- | returns all the possible lines that can have a winning streak for any board size.
--   uses diagsCycle to recursively find the diagonals at each distance from 
--   middle diagonals.
boardlines :: Board -> [[Maybe Player]]
boardlines board = rows ++ columns ++ diagonals board maxDistFromCenter
  where
    rows = board
    columns = transpose rows
    maxDistFromCenter = (length board) - minStreak

-- | given a Board-like structure - square [[]] - finds 4 diagonals lists per cycle.
--   it takes a board structure as argument and a distance from the central diagonals.
--   if distance is 0. Returns the middle diagonals twice therefore the @n == 0@ case.
--         A direction             B direction
--          x x 2 1 0               0 1 2 x x
-- Upside<--x 2 1 0 1               1 0 1 2 x-->Upside
--          2 1 0 1 2-->Downside    2 1 0 1 2
--          1 0 1 2 x    Downside<--x 2 1 0 1
--          0 1 2 x x               x x 2 1 0
diagonals :: [[a]] -> Int -> [[a]]
diagonals b n
  | n < 0     = []
  | n == 0    = take 2 $ fourDiags 0
  | otherwise = fourDiags n
  where
    fourDiags n = transpose [
      [ b !!  i    !! (n+i),  -- A direction Upside
        b !!  i    !! (l-i),  -- B direction Upside
        b !! (i+n) !!  i,     -- A direction Downside
        b !! (i+n) !! (l-i+n) -- B direction Downside
      ] | i <- [0..l]] ++ diagonals b (n-1)
    l = (length b) -1 - n
