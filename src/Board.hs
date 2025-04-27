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
boardlines board = rows ++ columns ++ diagonals
  where
    rows = board
    columns = transpose rows
    len = length rows - 1
    rowIxs = [0..len]
    colIxs = reverse rowIxs 
    maxDistFromCenter = (length board) - minimumWin
    diagonals = diagsCycle board maxDistFromCenter

-- x210  012x
-- 2101  1012
-- 1012  2101
-- 012x  x210
diagsCycle :: [[a]] -> Int -> [[a]]
diagsCycle b (-1) = []
diagsCycle b n = transpose [
  [ b !!  i    !! (n+i),
    b !! (i+n) !!  i,
    b !!  i    !! (l-i), 
    b !! (i+n) !! (l-i-n)
  ] | i <- [0..l]] ++ diagsCycle b (n-1)
  where
    l = (length b) -1 - n
