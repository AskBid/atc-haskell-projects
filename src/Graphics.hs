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
lineH []     = ""
lineH (p:ps) = "----" ++ lineH ps

pawnSpaces :: [Maybe Player] -> String
pawnSpaces [] = "|"
pawnSpaces (p:ps) = pawnSpace p ++ pawnSpaces ps
  where
    pawnSpace Nothing   = "|   "
    pawnSpace (Just O)  = "| O "
    pawnSpace (Just X)  = "| X "

boardRows :: Board -> [String]
boardRows b = upperSegment ++ ([lineH, pawnSpaces] <*> b)
  where 
    upperSegment = 
