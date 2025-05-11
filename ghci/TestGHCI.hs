module TestGHCI where

import Game
import Board
import Ai
import Helpers as H
import Data.Maybe

-- only for testing:
bb3 = [[Just X, Just O, Nothing]
      ,[Just X, Nothing, Just O]
      ,[Just O, Just X, Nothing]]

bb4 = [[Just X, Just O, Nothing, Just X]
      ,[Just X, Nothing, Just O, Nothing]
      ,[Just O, Just X,  Just X, Just O]
      ,[Just O, Just X, Nothing, Just O]]

bb9 = [ [Just X, Just O, Nothing, Just X,Nothing,Nothing,Nothing,Nothing,Just O]
       ,[Just X, Just O, Nothing, Just X,Nothing,Nothing,Nothing,Nothing,Just O]
       ,[Just X, Just O, Nothing, Just X,Nothing,Nothing,Nothing,Nothing,Just O]
       ,[Just X, Just O, Nothing, Just X,Nothing,Nothing,Nothing,Nothing,Just O]
       ,[Just X, Just O, Nothing, Just X,Nothing,Nothing,Nothing,Nothing,Just O]
       ,[Just X, Just O, Nothing, Just X,Nothing,Nothing,Nothing,Nothing,Just O]
       ,[Just X, Just O, Nothing, Just X,Nothing,Nothing,Nothing,Nothing,Just O]
       ,[Just X, Just O, Nothing, Just X,Nothing,Nothing,Nothing,Nothing,Just O]
       ,[Just X, Just O, Nothing, Just X,Nothing,Nothing,Nothing,Nothing,Just O]]

bb0 = [[Nothing, Nothing, Nothing]
      ,[Nothing, Nothing, Nothing]
      ,[Nothing, Nothing, Nothing]]


gm1 = (Game bb3 O 3)
gm0 = (Game bb0 O 3)

gm = Game {board = [[Nothing,Just O,Nothing],[Nothing,Just X,Nothing],[Nothing,Nothing,Nothing]]
                   , lastPlayer = X
                   , countToWin = 3
                   , evaporCount = Nothing
                   , movesHistory = Moves {playerO = [], playerX = []}}

b = board gm
bwc = boardWithCoords b
r0 = fromMaybe [] $ H.head (boardWithCoords b)





