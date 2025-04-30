{-# LANGUAGE OverloadedStrings #-}

-- | This module is used to visualise boards and games statics
module Graphics where

import Data.String (fromString)

import Board
import Helpers as H

--       A   B   C
--     -------------
--   1 |   |   |   | 1
--     -------------
--   2 | O |   | X | 2
--     -------------
--   3 |   |   |   | 3
--     -------------
--       A   B   C 

-- | prints the vertical edges of the pawns' cells, the pawn and/or the empty spaces.
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
boardRows [] = ["err: **empty board**"]
boardRows (row:rows) = colHs : edge : pawns row 1 : boardRows' rows 2 ++ [colHs]
  where
    edge = edgeH row
    colHs = colHeaders row
    boardRows' (row:[]) i   = edge : pawns row i : [edge]
    boardRows' (row:rows) i = edge : pawns row i : boardRows' rows (i+1)

-- | print Board for CLI terminal.
printBoard :: Board -> IO [()]
printBoard b = sequenceA $ putStrLn <$> (boardRows b) 

-- | Generates a string for the top/bottom of a printed Board, serving as a 
--   column header/index (in letters).
--   I added logic to handle spacing for multi-letter columns if the board size 
--   exceeds available letters,
--   though this case shouldn't occur. It was a good exercise!
--   It takes a Board row as input (any row works just as fine).
colHeaders :: [Maybe Player] -> String
colHeaders (p:ps) = "  " ++ strColH charsIxs'
  where
    l = length (p:ps)
    charsIxs' = take l $ charIndexes8 charsIxs
    maxSpaces = 4
    spacesR h = take (((maxSpaces - (length h)) `div` 2)) $ repeat ' '
    spacesL h 
      | odd $ length h = spacesR h ++ " " 
      | otherwise = spacesR h
    strColH [] = " "
    strColH (h:hs) = (spacesL h) ++ h ++ (spacesR h) ++ strColH hs

charsIxs :: [Char]
charsIxs = fromString "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- | creates an infinite list of letter indexes, multiple letters combo if exceedes 
--   number of chars available.
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
