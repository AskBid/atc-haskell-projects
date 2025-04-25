module Board where

import Data.List (transpose)

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

boardlines :: Board -> [[Maybe Player]]
boardlines rows = rows ++ columns ++ diagonals
  where
    columns = transpose rows
    len = length rows - 1
    diagonals = transpose [[rows !! r !! r, rows !! r !! (len - r)] | r <- rowIxs]
    rowIxs = [0..len]
    colIxs = reverse rowIxs

--  [[Just O, Just X, Nothing],
--  [Nothing, Just X, Nothing],
--  [Nothing, Just O, Just X]]
--
-- [[Just O,Just X,Nothing],
--  [Nothing,Just X,Nothing],
--  [Nothing,Just O,Just X],
--
--  [Just O,Nothing,Nothing],
--  [Just X,Just X,Just O],
--  [Nothing,Nothing,Just X],
--
--  [Just O,Nothing],
--  [Just O,Just X],
--  [Just O,Just O],
--  [Just X,Nothing],
--  [Just X,Just X],
--  [Just X,Nothing],
--  [Just X,Just X],
--  [Just X,Just O],
--  [Just X,Nothing]]
--
--
-- 1[1,2,3]
-- 2[1,2,3]
-- 3[1,2,3]
--
-- 1[3,2,1]
-- 2[3,2,1]
-- 3[3,2,1]
