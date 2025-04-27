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

boardlines :: Board -> [[Maybe Player]]
boardlines rows = rows ++ columns -- ++ diagonals
  where
    columns = transpose rows
    len = length rows - 1
    rowIxs = [0..len]
    colIxs = reverse rowIxs

-- x210  012x
-- 2101  1012
-- 1012  2101
-- 012x  x210
diagonals :: Board -> [[Maybe Player]]
diagonals b = diagsCycle b maxDistFromCenter ++ diagsCycle b (maxDistFromCenter - 1) 
  where 
    maxDistFromCenter = (length b) - minimumWin

diagsCycle :: [[a]] -> Int -> [[a]]
diagsCycle b n = transpose [
  [ b !!  i    !! (n+i),
    b !! (i+n) !!  i,
    b !!  i    !! (l-i), 
    b !! (i+n) !! (l-i-n)
  ] | i <- [0..l]]
  where
    l = (length b) -1 - n
