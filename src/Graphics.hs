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
pawnSpaces :: [Maybe Player] -> Int -> String
pawnSpaces [] i = "| " ++ show i
pawnSpaces (p:ps) i = pawnSpace p ++ pawnSpaces ps i
  where
    pawnSpace Nothing   = "|   "
    pawnSpace (Just O)  = "| O "
    pawnSpace (Just X)  = "| X "

-- | prints the upper and lower edge of the pawn's cells
edgeH :: [Maybe Player] -> String
edgeH []     = "-"
edgeH (p:ps) = "----" ++ edgeH ps

-- | gives and array of all the rows we need to print to visualise the board.
boardRows :: Board -> [String]
boardRows (row:rows) = edgeH row : pawnSpaces row 1 : boardRows' rows 1
  where
    edge = edgeH row
    boardRows' (row:[]) i   = edge : pawnSpaces row i : [edge]
    boardRows' (row:rows) i = edge : pawnSpaces row i : boardRows' rows (i+1)

hIndexs :: [Maybe Player] -> String
hIndexs (p:ps) = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
