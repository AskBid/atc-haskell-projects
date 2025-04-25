module Board where

import Data.List

data Player = O | X 
  deriving (Show, Eq)

type Board = [[Maybe Player]]

winLine :: [Maybe Player] -> Maybe Player
winLine (Nothing:_) = Nothing
winLine (p:ps) = foldl (\p' pSoFar -> if p' == pSoFar then p' else Nothing) p ps
-- winLine (p:ps) = foldl (\p' pSoFar -> samePlayer p pSoFar) p ps
  -- where
  --   samePlayer Nothing _ = Nothing
  --   samePlayer _ Nothing = Nothing
  --   samePlayer p' p''     | p' == p'' = p''
  --                         | otherwise = Nothing
-- could make accumulator to a counter (Bool, Int) in case of `win < lineLength`

winLines :: Board -> [[Maybe Player]]
winLines rows = rows ++ columns rows ++ diagonals rows
  where
    columns = transpose
    diagonals = undefined

-- row :: [Player] -> Bool
-- row ps = foldr (\p (p', b) ->  ) (Player, True) ps

-- 1[1,2,3]
-- 2[1,2,3]
-- 3[1,2,3]
--
--
