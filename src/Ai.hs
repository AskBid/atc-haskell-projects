-- | in this module I will mockup a rudimentary Ai for the sole purpose 
--   to test the interface with more ease, in the future I would like to
--   exercise with a search tree tecnique in Graham Hutton's book style
module Ai where

import Data.List (find)
import Test.LeanCheck ((><))

import Game
import Board

findAiMove :: Game -> Coordinate
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
                     Just xy -> xy
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
choseNextStreakCell p line = undefined
  where
    emptyAfter ((p,_):(Nothing,_):ns) = undefined 

choseNextStreakCell' :: Player -> [(Maybe Player, Int)] -> String-- Maybe Coordinate
choseNextStreakCell' p line = emptyAfter line 0
  where
    emptyAfter ((p',_):(Nothing,_):ns) c
      | p' == Just p = 
      | otherwise = 
    emptyAfter ((p',_):(p',_):ns) c
      | p' == Just p = emptyAfter ns (c+2)
      | otherwise =
    emptyAfter _ _ = "no"
-- xxxoe eoxxx
-- exoxx xxoxe
-- oxxex xexxo

findRandomEmpty :: [[(Maybe Player, Coordinate)]] -> Coordinate
findRandomEmpty b = undefined



-- listEmptyCells
--
-- boardlines
-- winStreak with countToWin-1
-- if any winstreak -1  found add +1 pawn if possible
-- could be recursive up to countWin-2,-1,-3?
