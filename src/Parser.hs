module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (elemIndex)
import Text.Read (readMaybe)

import Board

-- | parse coordinates from user input, forces to have the horizontal coordinate first,
--   but accepts both coordinates as numbers.
-- AB 1 --V
-- 1 AB --X
-- 3ab  --X
-- ba3  --V
-- 1 3  --V
coordinateParser :: [String] -> Parser Coordinate
coordinateParser charIndexes = do
  xs <- many1 digit <|> many1 letter
  spaces
  ys <- many1 digit
  case readMaybe xs of
    Just num -> return $ Coordinate {y=read ys, x=num}
    Nothing -> case fromCharIndexToInt charIndexes xs of
      Just x -> return $ Coordinate {x=x, y=read ys}
      Nothing -> fail "the letter coordinate was not valid."

digiCombo :: Parser Coordinate
digiCombo = do
  x <- many1 digit
  spaces
  y <- many1 digit
  return $ Coordinate {x=read x, y=read y}

digiCharCombo :: [String] -> Parser Coordinate
digiCharCombo cxs = do
  cx <- many1 letter
  spaces
  y <- many1 digit
  case fromCharIndexToInt cxs cx of
    Nothing -> fail "the letter coordinate was not valid."
    Just x' -> return $ Coordinate {x= x', y=read y}

fromCharIndexToInt :: [String] -> String -> Maybe Int
fromCharIndexToInt cixs cix = (+1) <$> (elemIndex cix cixs) 
