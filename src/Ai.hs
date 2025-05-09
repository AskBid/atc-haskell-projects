-- | in this module I will mockup a rudimentary Ai for the sole purpose 
--   to test the interface with more ease, in the future I would like to
--   exercise with a search tree tecnique in Graham Hutton's book style
module Ai where

import Game
import Board

makeAiMove :: Game -> Game
makeAiMove gm = undefined

-- listEmptyCells

-- boardlines
-- winStreak with countToWin-1
-- if any winstreak -1  found add +1 pawn if possible
-- could be recursive up to countWin-2,-1,-3?
