module Board where

import Data.List (transpose)

data Player = O | X 
  deriving (Show, Eq)

type Board = [[Maybe Player]]

mkBoard :: Int -> Board
mkBoard n = take n $ repeat (take n $ repeat Nothing)

winLine :: [Maybe Player] -> Maybe Player
winLine (Nothing:_) = Nothing
winLine (p:ps) = foldl compare p ps
  where
    compare p' pSoFar 
      | p' == pSoFar = p' 
      | otherwise    = Nothing

boardlines :: Board -> [[Maybe Player]]
boardlines rows = rows ++ columns ++ diagonals
  where
    columns = transpose rows
    len = length rows - 1
    diagonals = transpose [[rows !! r !! r, rows !! r !! (len - r)] | r <- rowIxs]
    rowIxs = [0..len]
    colIxs = reverse rowIxs

--
-- 1[1,2,3]
-- 2[1,2,3]
-- 3[1,2,3]
--
-- 1[3,2,1]
-- 2[3,2,1]
-- 3[3,2,1]
