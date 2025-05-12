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

gm2 = Game {board = [[Just O,Nothing,Nothing],[Nothing,Just X,Nothing],[Nothing,Nothing,Nothing]]
                    , lastPlayer = O, countToWin = 3
                    , evaporCount = Nothing
                    , movesHistory = Moves {playerO = [], playerX = []}}

gm3 = Game {board = [[Just X,Just O,Nothing],[Just O,Nothing,Nothing],[Just X,Nothing,Nothing]]
                    , lastPlayer = O, countToWin = 3
                    , evaporCount = Nothing
                    , movesHistory = Moves {playerO = [], playerX = []}}

b = board gm
-- bwc = boardWithCoords b
-- r0 = fromMaybe [] $ H.head bwc

r1 = [(Just O,Coordinate {x = 1, y = 1})
     ,(Nothing,Coordinate {x = 2, y = 2})
     ,(Just O,Coordinate {x = 3, y = 3})
     ,(Just O,Coordinate {x = 4, y = 4})
     ,(Nothing,Coordinate {x = 5, y = 5})
     ]

r1b = [(Just O,Coordinate {x = 0, y = 1})
     ,(Nothing,Coordinate {x = 0, y = 2})
     ,(Just O,Coordinate {x = 0, y = 3})
     ,(Just O,Coordinate {x = 0, y = 4})
     ,(Nothing,Coordinate {x = 0, y = 5})
     ]

r2 = [(Just O,Coordinate {x = 0, y = 1})
     ,(Just O,Coordinate {x = 0, y = 2})
     ,(Nothing,Coordinate {x = 0, y = 3})
     ,(Just O,Coordinate {x = 0, y = 4})
     ,(Just O,Coordinate {x = 0, y = 5})
     ,(Nothing,Coordinate {x = 0, y = 6})
     ]

r3 = [(Just O,Coordinate {x = 0, y = 3})
     ,(Just O,Coordinate {x = 0, y = 4})
     ,(Nothing,Coordinate {x = 0, y = 5})
     ]

r4 = [(Just O,Coordinate {x = 0, y = 3})
     ,(Nothing,Coordinate {x = 0, y = 4})
     ,(Nothing,Coordinate {x = 0, y = 5})
     ]


