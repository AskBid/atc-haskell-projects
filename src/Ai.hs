-- | in this module I will mockup a rudimentary Ai for the sole purpose 
--   to test the interface with more ease, in the future I would like to
--   exercise with a search tree tecnique in Graham Hutton's book style
module Ai where

import Game
import Board

makeAiMove :: Game -> Game
makeAiMove gm = undefined
  where 
    b = board gm
    ls = boardlines b
    p = otherPlayer $ lastPlayer gm

makeCoordsBoard :: Board -> [[Coordinate]]
makeCoordsBoard b = undefined

boardAiLines :: Board -> [[Coordinate]] -> [([Maybe Player], [Coordinate])]
boardAiLines b bc = undefined

findStreaks :: Player -> Int -> ([Maybe Player],[Coordinate]) -> Bool
findStreaks p ctw (ps,cs) = 
  case winStreak ctw ps of
    p -> True

findMove :: Player -> ([Maybe Player],[Coordinate]) -> Coordinate
findMove p line = undefined 



-- listEmptyCells
--
-- boardlines
-- winStreak with countToWin-1
-- if any winstreak -1  found add +1 pawn if possible
-- could be recursive up to countWin-2,-1,-3?
