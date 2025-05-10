-- | in this module I will mockup a rudimentary Ai for the sole purpose 
--   to test the interface with more ease, in the future I would like to
--   exercise with a search tree tecnique in Graham Hutton's book style
module Ai where

import Data.List (find)

import Game
import Board

findAiMove :: Game -> Coordinate
findAiMove gm = findAiMove' $ countToWin gm
  where 
  findAiMove' 0    = findRandomEmpty aiLines
  findAiMove' ctw = 
    case find (findStreaks p ctw) aiLines of
      Just line -> case choseNextStreakCell p line of
                     Nothing -> findRandomEmpty aiLines
                     -- ^should really look for possible other 
                     --  incomplete streak, but I will keep it
                     --  simple for this round.
                     Just xy -> xy
      otherwise -> findAiMove' $ ctw - 1
  b = board gm
  ls = boardlines b
  p = otherPlayer $ lastPlayer gm
  cb = makeCoordsBoard b 
  aiLines = boardAiLines b cb

makeCoordsBoard :: Board -> [[Coordinate]]
makeCoordsBoard b = undefined

boardAiLines :: Board -> [[Coordinate]] -> [([Maybe Player], [Coordinate])]
boardAiLines b bc = undefined

findStreaks :: Player -> Int -> ([Maybe Player],[Coordinate]) -> Bool
findStreaks p ctw (ps,cs) = 
  case winStreak ctw ps of
    p -> True

choseNextStreakCell :: Player -> ([Maybe Player],[Coordinate]) -> Maybe Coordinate
choseNextStreakCell p line = undefined

findRandomEmpty :: [([Maybe Player], [Coordinate])] -> Coordinate
findRandomEmpty b = undefined



-- listEmptyCells
--
-- boardlines
-- winStreak with countToWin-1
-- if any winstreak -1  found add +1 pawn if possible
-- could be recursive up to countWin-2,-1,-3?
