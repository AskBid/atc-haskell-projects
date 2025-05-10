-- | in this module I will mockup a rudimentary Ai for the sole purpose 
--   to test the interface with more ease, in the future I would like to
--   exercise with a search tree tecnique in Graham Hutton's book style
module Ai where

import Data.List (find)
import Test.LeanCheck ((><))
import Data.Maybe (fromMaybe, isNothing)

import Game
import Board

findAiMove :: Game -> Maybe Coordinate
findAiMove gm = findAiMove' $ countToWin gm
  where 
  findAiMove' 0    = findRandomEmpty bwc
  findAiMove' ctw = 
    case find (findQuasiStreaks p ctw) bwc of
      Just line -> case choseNextStreakCell p line of
                     Nothing -> findRandomEmpty bwc
                     -- ^should really look for possible other 
                     --  incomplete streak, but I will keep it
                     --  simple for this round.
                     Just xy -> Just xy
      otherwise -> findAiMove' $ ctw - 1
  b = board gm
  ls = boardlines b
  p = otherPlayer $ lastPlayer gm
  cb = mkCoordsBoard $ length b 
  bwc = boardWithCoords b cb

mkCoordsBoard :: Int -> [[Coordinate]]
mkCoordsBoard l = (makeLine l) <$> [0..l]
  where 
    makeLine l row = [Coordinate row n | n <- [0..l]]

-- | proudly found (><) just by looking up the type on hoogle *o*
boardWithCoords :: Board -> [[Coordinate]] -> [[(Maybe Player, Coordinate)]]
boardWithCoords b bc = (><) b bc

findQuasiStreaks :: Player -> Int -> [(Maybe Player, Coordinate)] -> Bool
findQuasiStreaks p ctw rws = 
  case winStreak ctw ps of
    p -> True
  where 
    ps = fst $ unzip rws

choseNextStreakCell :: Player -> [(Maybe Player, Coordinate)] -> Maybe Coordinate
choseNextStreakCell p line = emptyCell $ findCoord $ maxCount counts
  where
    emptyAfter :: [(Maybe Player, Coordinate)] -> Int -> [(Coordinate, Int)]
    emptyAfter [] _ = []
    emptyAfter ((p',_):(Nothing,coord):ns) c
      | p' == Just p = (coord,c+1) : (emptyAfter ns 0) 
      | otherwise = emptyAfter ns 0 
    emptyAfter ((p',_):(b',_):ns) c
      | p' == Just p && b'==p' = emptyAfter ns (c+2)
      | otherwise = emptyAfter ns 0
    emptyAfter _ _ = []
    streaks = emptyAfter line 0
    counts = snd $ unzip streaks
    maxCount [] = Nothing
    maxCount ns = Just $ maximum ns
    findCoord Nothing    = Nothing
    findCoord (Just max) = find (\(_,count) -> max == count) streaks
    emptyCell Nothing = Nothing
    emptyCell (Just (coo,_)) = Just coo

findRandomEmpty :: [[(Maybe Player, Coordinate)]] -> Maybe Coordinate
findRandomEmpty b = sortResult findEmpties 
  where 
    findEmpties = filter (\(cell,_) -> isNothing cell) $ concat b
    sortResult []      = Nothing
    sortResult ((_,c):_) = Just c


-- listEmptyCells
--
-- boardlines
-- winStreak with countToWin-1
-- if any winstreak -1  found add +1 pawn if possible
-- could be recursive up to countWin-2,-1,-3?
