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
boardRows [] = ["err:**empty board**"]
boardRows (row:rows) = colHs : edge : pawns row 1 : boardRows' rows 2 ++ [colHs]
  where
    edge = edgeH row
    colHs = colHeaders row
    boardRows' (row:[]) i   = edge : pawns row i : [edge]
    boardRows' (row:rows) i = edge : pawns row i : boardRows' rows (i+1)

printBoard :: Board -> IO [()]
printBoard b = sequenceA $ putStrLn <$> (boardRows b) 

colHeaders :: [Maybe Player] -> String
colHeaders (p:ps) = "  " ++ strColH charsIxs'
  where
    l = length (p:ps)
    charsIxs' = take l $ charIndexes8 charsIxs
    maxSpaces = 5
    spacesL h = take ((maxSpaces - (length h)) `div` 2) $ repeat ' '
    spacesR h 
      | odd $ length h = spacesL h 
      | otherwise = spacesL h ++ " "
    strColH [] = " "
    strColH (h:hs) = (spacesL h) ++ h ++ (spacesR h) ++ strColH hs

charsIxs :: [Char]
charsIxs = ['A','B','C','D','E','F','G','H','I','J','K','L','M'
           ,'N','O','P','Q','R','S','T','U','V','W','X','Y','Z']

charIndexes8 :: [Char] -> [String]
charIndexes8 cs = charIndex cs <$> [0..] 

charIndex :: [Char] -> Int -> String
charIndex cs i = (prefix (i `div` l)) ++ [suffixChar]
  where
    suffixChar = cs !! (i `mod` l)
    l = length cs
    prefix i'
      | i < l = ""
      | i' <= l = [cs !! ((i'-1) `mod` l)]
      | otherwise = charIndex cs i'
