module Board where

import Data.List (transpose)
import Debug.Trace (trace)

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
  | ltw <= board && ltw > 1 = Just $ Game (mkBoard board) O ltw
  | otherwise               = Nothing

mkBoard :: Int -> Board
mkBoard n = take n $ repeat (take n $ repeat Nothing)

winLineLtw :: Game -> [Maybe Player] -> Maybe Player
winLineLtw _ [] = Nothing
winLineLtw (Game _ _ ltw) (p:ps) = if fst mayWin >= ltw then snd mayWin else Nothing
  where
    compare (count, pSoFar) p'
      | count >= ltw = (ltw, pSoFar)
      | null p'      = (1, p')
      | p' == pSoFar = (count + 1, p')
      | otherwise    = (1, p')
    mayWin = foldl compare (1,p) ps

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
