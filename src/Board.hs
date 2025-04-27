module Board where

import Data.List (transpose)
import Data.Maybe (isNothing)

data Player = O | X 
  deriving (Show, Eq)

type Board = [[Maybe Player]]

mkBoard :: Int -> Board
mkBoard n = take n $ repeat (take n $ repeat Nothing)

boardlines :: Board -> [[Maybe Player]]
boardlines rows = rows ++ columns ++ diagonals
  where
    columns = transpose rows
    len = length rows - 1
    diagonals = transpose [[rows !! r !! r, rows !! r !! (len - r)] | r <- rowIxs]
    rowIxs = [0..len]
    colIxs = reverse rowIxs
