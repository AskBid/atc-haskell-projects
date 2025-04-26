module Board where

import Data.List (transpose)

data Player = O | X 
  deriving (Show, Eq)

type Board = [[Maybe Player]]

data Game = Game
  { board       :: Board 
  , turn        :: Player
  , lengthToWin :: Int
  }

mkGame :: Int -> Int -> Maybe Game
mkGame board ltw 
  | ltw <= board = Game (mkBoard board) O ltw
  | otherwise    = Nothing

mkBoard :: Int -> Board
mkBoard n = take n $ repeat (take n $ repeat Nothing)

winLine :: [Maybe Player] -> Maybe Player
winLine (Nothing:_) = Nothing
winLine (p:ps) = foldl compare p ps
  where
    compare p' pSoFar 
      | p' == pSoFar = p' 
      | otherwise    = Nothing

boardlines :: Board -> [[Maybe Player]]
boardlines rows = rows ++ columns ++ diagonals
  where
    columns = transpose rows
    len = length rows - 1
    diagonals = transpose [[rows !! r !! r, rows !! r !! (len - r)] | r <- rowIxs]
    rowIxs = [0..len]
    colIxs = reverse rowIxs


bb2 = [[Just X, Just O, Nothing]
      ,[Just X, Nothing, Just O]
      ,[Just O, Just X, Nothing]]

bb3 = [[Just X, Just O, Nothing, Just X]
      ,[Just X, Nothing, Just O, Nothing]
      ,[Just O, Just X,  Just X, Just O]
      ,[Just O, Just X, Nothing, Just O]]
