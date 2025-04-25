module Board where

import Data.List

data Player = O | X

type Board = [[Maybe Player]]

win :: Board -> _
win ps = rows . cols . diag

rows :: Board -> [[Maybe Player]]
rows rs = rs ++ transpose rs ++ diagonals rs
  where
    diagonals = undefined

-- row :: [Player] -> Bool
-- row ps = foldr (\p (p', b) ->  ) (Player, True) ps


