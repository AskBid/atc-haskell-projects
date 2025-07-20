{-# LANGUAGE OverloadedStrings #-}
-- | in this module I will mockup a rudimentary Ai for the sole purpose 
--   to test the interface with more ease, in the future I would like to
--   exercise with a search tree tecnique in Graham Hutton's book style
module Ai (findAiMove) where

import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)
import System.Random

import Game 
  ( Game
  , winStreak
  , lastPlayer
  , otherPlayer
  , board
  , countToWin
  )
import Board 
  ( Coordinate(..)
  , Player
  , Board
  , boardlines
  )
import Helpers ((><))

findAiMove :: Game -> IO Coordinate
findAiMove gm = findAiMove' $ countToWin gm
  where 
  findAiMove' 1   = findRandomEmpty bwc
  findAiMove' ctw = case find (isQuasiStreaks p ctw) bwc of
      Just line -> case choseNextStreakCell (Just p) line of
                     Nothing -> findRandomEmpty bwc
                     -- ^should really look for possible other 
                     --  incomplete streak, but I will keep it
                     --  simple for this round.
                     Just xy -> pure xy
      Nothing   -> do
        n <- randomNumber 1
        case n of
          1 -> findAiMove' $ ctw - 1
          0 -> findRandomEmpty bwc
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

isQuasiStreaks :: Player -> Int -> [(Maybe Player, Coordinate)] -> Bool
isQuasiStreaks p ctw rws = 
  case winStreak ctw ps of
    Nothing -> False
    Just p'  -> p == p' 
  where 
    ps = fst $ unzip rws

choseNextStreakCell :: Maybe Player -> [(Maybe Player, Coordinate)] -> Maybe Coordinate
choseNextStreakCell p line = emptyCell $ findCoord $ maxCount counts
  where
    streaks = rankEmptysAfterP p line 0 ++ rankEmptysAfterP p (reverse line) 0
    counts = snd $ unzip streaks
    maxCount [] = Nothing
    maxCount ns = Just $ maximum ns
    findCoord Nothing    = Nothing
    findCoord (Just max) = find (\(_,count) -> max == count) streaks
    emptyCell Nothing = Nothing
    emptyCell (Just (coo,_)) = Just coo

rankEmptysAfterP :: Maybe Player -> [(Maybe Player, Coordinate)] -> Int -> [(Coordinate, Int)]
rankEmptysAfterP p row counter = rankEmptysAfterP' row counter 
  where
    rankEmptysAfterP' :: [(Maybe Player, Coordinate)] -> Int -> [(Coordinate, Int)]
    rankEmptysAfterP' [] _ = []
    rankEmptysAfterP' ((p',_):(Nothing,coord):ns) c
      | p' == p = (coord,c+1) : (rankEmptysAfterP' ns 0) 
      | otherwise = rankEmptysAfterP' ns 0 
    rankEmptysAfterP' ((p',_):ns) c
      | p' == p = rankEmptysAfterP' ns (c+1)
      | otherwise = rankEmptysAfterP' ns 0

-- | in theory a board without empty space will never be fed here
--   but otherwise need to sort out the edge case for @(!!)@
findRandomEmpty :: [[(Maybe Player, Coordinate)]] -> IO Coordinate
findRandomEmpty b = do
  let emptyCells = filter (\(cell,_) -> isNothing cell) $ concat b
  let len = (length emptyCells) - 1 
  rn <- randomNumber len 
  empty <- pure (emptyCells !! rn)
  return $ extract empty
    where
      extract :: (Maybe Player, Coordinate) -> Coordinate 
      extract (_,xys) = xys

randomNumber :: Int -> IO Int
randomNumber mx = do
  n <- randomRIO (0, mx)
  return n
