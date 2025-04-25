module Board where

data Player = O | X

type Board = [[Maybe Player]]

-- win :: Board -> Player
-- win ps = rows . cols . diag
--   where 
--     rows = row <$> ps
--     row = all 


