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


bb2 = [[Just X, Just O, Nothing]
      ,[Just X, Nothing, Just O]
      ,[Just O, Just X, Nothing]]

-- calculating smaller diagonals as well.
-- [0 0, 1 1, 2 2]
-- [0 2, 1 1, 2 0]
-- [0 1, 1 0] 
-- [1 2, 2 1]
-- [0 0]
-- [2 2]

bb3 = [[Just X, Just O, Nothing, Just X]
      ,[Just X, Nothing, Just O, Nothing]
      ,[Just O, Just X,  Just X, Just O]
      ,[Just O, Just X, Nothing, Just O]]

-- len = 3
-- [0 0, 1 1, 2 2, 3 3]              0 1 2 3 -- 0 1 2 3
-- i=[0..len]                        i          i
-- [0 3, 1 2, 2 1, 3 0]              0 1 2 3 -- 3 2 1 0 
--                                   i          len - i
--
-- [0 2, 1 1, 2 0]                   0 1 2   -- 2 1 0
-- i=[0..len-1]                      i          len - (i + 1)       
-- [1 3, 2 2, 3 1]                   1 2 3   -- 3 2 1 
--                                   i+1        len - i
--
--
-- [0 1, 1 0]                        0 1     -- 1 0
-- i=[0..len-2]                      i          
-- [2 3, 3 2]                        2 3        3 2
--
-- [0 0]
-- [3 3]
--


