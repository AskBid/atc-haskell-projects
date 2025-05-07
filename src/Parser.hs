module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (elemIndex)
import Text.Read (readMaybe)
import Data.Char

import Board

-- | parse coordinates from user input, forces to have the horizontal coordinate first,
--   but accepts both coordinates as numbers.
--   first argument is the list of all possible board letter indexes 
--   (limited by board size).
-- AB 1 --V
-- 1 AB --X
-- 3ab  --X
-- ba3  --V
-- 1 3  --V
coordinateParser :: [String] -> Parser Coordinate
coordinateParser charIndexes = do
  coords <- digiCombo <|> charDigiCombo charIndexes
  return coords

-- | parses only two digits input with a space in between
digiCombo :: Parser Coordinate
digiCombo = do
  x <- many1 digit
  spaces
  y <- many1 digit
  return $ Coordinate {x=read x, y=(read y)-1}

-- | parses only a letter index plus a digit input.
--   first argument is the list of all possible board letter indexes 
--   (limited by board size). 
charDigiCombo :: [String] -> Parser Coordinate
charDigiCombo cxs = do
  cx <- many1 letter
  spaces
  y <- many1 digit
  case fromCharIndexToInt cxs (toUpper <$> cx) of
    Nothing -> fail "the letter coordinate was not valid."
    Just x' -> return $ Coordinate {x= x', y=(read y)-1}


-- | from a list of letter indexes for the current board, gives the corresponding
--   integer index.
--   first argument is the list of all possible board letter indexes 
--   (limited by board size).
fromCharIndexToInt :: [String] -> String -> Maybe Int
fromCharIndexToInt cixs cix = elemIndex cix cixs
