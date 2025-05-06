module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

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
  return $ Coordinate {x=fromCharIndexToInt charIndexes xs, y=read ys}

fromCharIndexToInt :: [String] -> String -> Int
fromCharIndexToInt str = undefined
