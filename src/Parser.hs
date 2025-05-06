module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Board

coordinateParser :: Parser Coordinate
coordinateParser = do
  xs <- many1 digit <|> many1 letter
  spaces
  ys <- many1 digit
  return $ Coordinate {y=read xs, x=read ys} 
-- AB 1 --V
-- 1 AB --X
-- 3ab  --X 
-- ba3  --V
-- 1 3  --V
