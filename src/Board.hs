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
  | ltw <= board = Just $ Game (mkBoard board) O ltw
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

winLineLtw :: Int -> [Maybe Player] -> (Int, Maybe Player)
winLineLtw _ []       = (1, Nothing)
winLineLtw ltw (p:ps) = foldl compare (1,p) ps
  where
    compare (count, pSoFar) p'
      | count >= ltw = trace ("compare (" ++ show count ++ ", " ++ (show pSoFar) ++ ") " ++ (show p') ++ " count >= ltw") (ltw, p')
      | null p'      = (1, p')
      | p' == pSoFar = (count + 1, p')
      | otherwise    = (1, p')
      -- | count >= ltw = trace ("prev: " ++ (show pSoFar) ++ " " ++ (show count) ++ " - next: " ++ (show p')) (ltw, p')
      -- | null p'      = trace ("Nothing, so back to 1 and " ++ p') (1, p')
      -- | p' == pSoFar = trace ("prev: " ++ (show pSoFar) ++ " " ++ (show count) ++ " - next: " ++ (show p') ++ " so: " ++ (show $ count+1)) (count + 1, p')
      -- | otherwise    = trace ("otherwise, back to 1 and " ++ p') (1, p')

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
