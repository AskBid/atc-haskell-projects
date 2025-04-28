module Board where

import Data.List (transpose)
import Data.Maybe (isNothing)

data Player = O | X 
  deriving (Show, Eq)

type Board = [[Maybe Player]]

minimumWin :: Int
minimumWin = 3

mkBoard :: Int -> Board
mkBoard n = take n $ repeat (take n $ repeat Nothing)

-- | returns all the possible lines that can have a winning streak for any board size.
--   uses diagsCycle to recursively find the diagonals at each distance from 
--   middle diagonals (which are doubled).
boardlines :: Board -> [[Maybe Player]]
boardlines board = rows ++ columns ++ diagonals
  where
    rows = board
    columns = transpose rows
    maxDistFromCenter = (length board) - minimumWin
    diagonals = diagsCycle board maxDistFromCenter

--         A direction             B direction
--
--          x x 2 1 0               0 1 2 x x
-- Upside<--x 2 1 0 1               1 0 1 2 x-->Upside
--          2 1 0 1 2-->Downside    2 1 0 1 2
--          1 0 1 2 x    Downside<--x 2 1 0 1
--          0 1 2 x x               x x 2 1 0
--
-- | given a Board-like structure - [[]] - finds 4 diagonals lists.
--   it takes a board structure as argument and a distance from the central diagonals.
--   if distance is 0, returns the middle diagonals twice.
diagsCycle :: [[a]] -> Int -> [[a]]
diagsCycle b (-1) = []
diagsCycle b n = transpose [
  [ b !!  i    !! (n+i),  -- A direction Upside
    b !! (i+n) !!  i,     -- A direction Downside
    b !!  i    !! (l-i),  -- B direction Upside
    b !! (i+n) !! (l-i-n) -- B direction Downside
  ] | i <- [0..l]] ++ diagsCycle b (n-1)
  where
    l = (length b) -1 - n
