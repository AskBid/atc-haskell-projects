{-# LANGUAGE OverloadedStrings #-}

-- | This module is used to visualise boards and games statics
module Graphics where

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
--   First argument is a row from Board, while the Int is the index number to be
--   printed outside the board.
pawns :: [Maybe Player] -> Int -> String
pawns (p:ps) i = show i ++ " " ++ pawn p ++ pawns' ps ++ " " ++ show i
  where
    pawns' [] = "|"
    pawns' (p:ps)  = pawn p ++ pawns' ps
    pawn Nothing   = "|   "
    pawn (Just O)  = "| O "
    pawn (Just X)  = "| X "

-- | prints the upper and lower edge of the pawn's cells
edgeH :: [Maybe Player] -> String
edgeH ps = "  " ++ edgeH' ps
  where
    edgeH' []     = "-"
    edgeH' (p:ps) = "----" ++ edgeH' ps

-- | gives and array of all the rows we need to print to visualise the board.
boardRows :: Board -> [String]
boardRows (row:rows) = edgeH row : pawns row 1 : boardRows' rows 2
  where
    edge = edgeH row
    boardRows' (row:[]) i   = edge : pawns row i : [edge]
    boardRows' (row:rows) i = edge : pawns row i : boardRows' rows (i+1)

hIndexs :: [Maybe Player] -> String
hIndexs (p:ps) = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
