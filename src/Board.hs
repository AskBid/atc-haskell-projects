module Board where

import Data.List

data Player = O | X 
  deriving (Show, Eq)

type Board = [[Maybe Player]]

winLine :: [Maybe Player] -> Maybe Player
winLine (Nothing:_) = Nothing
winLine (p:ps) = foldl compare p ps
  where
    compare p' pSoFar 
      | p' == pSoFar = p' 
      | otherwise    = Nothing

winLines :: Board -> [[Maybe Player]]
winLines rows = rows ++ columns ++ diagonals
  where
    columns = transpose rows
    diagonals = [[rows !! row !! 0, rows !! row !! col] | row <- [0..length rows], col <- reverse [0..length rows]]
    rowIxs = [0..length rows]
    colIxs = reverse rowIxs

-- 1[1,2,3]
-- 2[1,2,3]
-- 3[1,2,3]
--
-- 1[3,2,1]
-- 2[3,2,1]
-- 3[3,2,1]
