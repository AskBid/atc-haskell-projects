module Game 
  ( Game 
  , mkGame
  ) where

import Data.Maybe

import Board

data Game = Game
  { board       :: Board 
  , turn        :: Player
  , countToWin  :: Int
  }

mkGame :: Int -> Int -> Maybe Game
mkGame boardL ctw 
  | ctw <= boardL && ctw > 1 = Just $ Game (mkBoard boardL) O ctw
  | otherwise                = Nothing

-- |
-- Qs: is using Game bad for performance Vs only Int? 
--     trade off with clarity/solidity?
winStreak :: Game -> [Maybe Player] -> Maybe Player
winStreak _ [] = Nothing
winStreak game (p:ps) = if fst maxStreak >= ctw 
                        then snd maxStreak 
                        else Nothing
  where
    ctw = countToWin game
    maxStreak = foldl compare (1, p) ps
    compare (count, p) p'
      | count >= ctw = (count, p)
      | isNothing p' = (1, p')
      | p' == p      = (count+1, p')
      | otherwise    = (1, p')

