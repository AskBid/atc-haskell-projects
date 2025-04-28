{-# LANGUAGE OverloadedStrings #-}

-- | This module is used to visualise boards and games statics
module Graphics where

import Data.Maybe (fromMaybe)

import Board
import Helpers as H

--    -------------
--    |   |   |   |
--    -------------
--    | O |   | X |
--    -------------
--    |   |   |   |
--    -------------


-- | prints the vertical edges of the pawns' cells, the pawn and/or empty sapces.
pawnSpaces :: [Maybe Player] -> String
pawnSpaces [] = "|"
pawnSpaces (p:ps) = pawnSpace p ++ pawnSpaces ps
  where
    pawnSpace Nothing   = "|   "
    pawnSpace (Just O)  = "| O "
    pawnSpace (Just X)  = "| X "

-- | prints the upper and lower edge of the pawn's cells
edgeH :: String -> String
edgeH s = take (length s) $ repeat '-'

-- | gives and array of all the rows we need to print to visualise the board.
boardRows :: Board -> [String]
boardRows [] = []
boardRows (row:rows) = edgeH cells : cells : boardRows rows ++ [edgeH cells]
  where 
    cells = pawnSpaces row
