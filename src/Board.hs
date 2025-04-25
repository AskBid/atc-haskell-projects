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
winLines rows = rows ++ columns rows ++ diagonals rows
  where
    columns = transpose
    diagonals = undefined

-- 1[1,2,3]
-- 2[1,2,3]
-- 3[1,2,3]
--
--
