{-# LANGUAGE OverloadedStrings #-}

-- | This module is used to visualise boards and games statics
module Graphics where

import Board

--    -------------
--    |   |   |   |
--    -------------
--    | O |   | X |
--    -------------
--    |   |   |   |
--    -------------

lineH :: [Maybe Player] -> String
lineH b = line (length b)
  where
    line 0 = ""
    line count = segment ++ line (count-1)
    segment = "----"

pawnSpaces :: [Maybe Player] -> String
pawnSpaces [] = "|"
pawnSpaces (p:ps) = pawnSpace p ++ pawnSpaces ps
  where
    pawnSpace Nothing   = "|   "
    pawnSpace $ Just O  = "| O "
    pawnSpace $ Just X  = "| X "
