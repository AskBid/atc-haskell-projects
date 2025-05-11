-- | in this module I will mockup a rudimentary Ai for the sole purpose 
--   to test the interface with more ease, in the future I would like to
--   exercise with a search tree tecnique in Graham Hutton's book style
module Ai where

import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)
import System.Random

import Game
import Board
import Helpers ((><))

findAiMove :: Game -> Maybe Coordinate
findAiMove gm = findAiMove' $ countToWin gm
  where 
  findAiMove' 0   = findRandomEmpty bwc
  findAiMove' ctw = case find (findQuasiStreaks p ctw) bwc of
      Just line -> case choseNextStreakCell (Just p) line of
                     Nothing -> findRandomEmpty bwc
                     -- ^should really look for possible other 
                     --  incomplete streak, but I will keep it
                     --  simple for this round.
                     Just xy -> Just xy
      otherwise -> findAiMove' $ ctw - 1
  b = board gm
  ls = boardlines b
  p = otherPlayer $ lastPlayer gm
  bwc = boardWithCoords b

mkCoordsBoard :: Int -> [[Coordinate]]
mkCoordsBoard l = (makeLine l) <$> [0..l-1]
  where 
    makeLine l row = [Coordinate {y=row, x=n} | n <- [0..l-1]]

-- | (><) :: [[a]] -> [[b]] -> [[(a,b)]] from Helpers
boardWithCoords :: Board -> [[(Maybe Player, Coordinate)]]
boardWithCoords b = (><) b bc
  where 
    bc = mkCoordsBoard $ length b

findQuasiStreaks :: Player -> Int -> [(Maybe Player, Coordinate)] -> Bool
findQuasiStreaks p ctw rws = 
  case winStreak ctw ps of
    p -> True
  where 
    ps = fst $ unzip rws

choseNextStreakCell :: Maybe Player -> [(Maybe Player, Coordinate)] -> Maybe Coordinate
choseNextStreakCell p line = emptyCell $ findCoord $ maxCount counts
  where
    streaks = emptyAfter p line 0 ++ emptyAfter p (reverse line) 0
    counts = snd $ unzip streaks
    maxCount [] = Nothing
    maxCount ns = Just $ maximum ns
    findCoord Nothing    = Nothing
    findCoord (Just max) = find (\(_,count) -> max == count) streaks
    emptyCell Nothing = Nothing
    emptyCell (Just (coo,_)) = Just coo

emptyAfter :: Maybe Player -> [(Maybe Player, Coordinate)] -> Int -> [(Coordinate, Int)]
emptyAfter p row counter = emptyAfter' row counter 
  where
    emptyAfter' :: [(Maybe Player, Coordinate)] -> Int -> [(Coordinate, Int)]
    emptyAfter' [] _ = []
    emptyAfter' ((p',_):(Nothing,coord):ns) c
      | p' == p = (coord,c+1) : (emptyAfter' ns 0) 
      | otherwise = emptyAfter' ns 0 
    emptyAfter' ((p',_):ns) c
      | p' == p = emptyAfter' ns (c+1)
      | otherwise = emptyAfter' ns 0

findRandomEmpty :: [[(Maybe Player, Coordinate)]] -> IO (Maybe Coordinate)
findRandomEmpty b = do
  let emptyCells = filter (\(cell,_) -> isNothing cell) $ concat b
  let len = length emptyCells 
  rn <- randomNumber len 
  empty <- pure (emptyCells !! rn)
  return $ extract emptyCells
    where
      extract :: [(Maybe Player, Coordinate)] -> Maybe Coordinate
      extract []           = Nothing 
      extract ((_,xys):as) = Just xys

randomNumber :: Int -> IO Int
randomNumber mx = do
  n <- randomRIO (0, mx)
  return n
