module Interface.Settings where

import Control.Monad.State
import Text.Parsec

import Interface.AppState
import Interface.Helpers
import Parser
import Game

-- | CLI dialogues to take user's input on creating differnet board size and 
--   game parameters.
buildCustomGame :: StateT AppState IO ()
buildCustomGame = do 
  boardSize <- liftIO $ getSettingSize 3 "Enter board size:" 3 100 
  printLn $ "Board size: " ++ show boardSize
  countToWin <- liftIO $ getSettingSize 3 "Enter winning strake amount:" 3 boardSize
  printLn $ "Winning strake amount: " ++ show countToWin
  let gm = mkGame boardSize countToWin
  evaporCount <- liftIO $ getSettingSize countToWin msgEC countToWin $ (boardSize^2) `div` 2
  printLn $ "Evaporating after?: " ++ show evaporCount 
  modify (\s -> s {game= (gm{evaporCount= Just evaporCount})})
  printLn "Custom Game Started!"
  printLn $ "Make a line of "++ show countToWin ++" pawns to win the game!" 
    where 
      msgEC = "                                                            \n\
              \Evaporating or Standard game Setup:\\/\\/\\/                \n\
              \Enter how many moves before the pawns start to evaporate.   \n\
              \(it needs to be more than the winning streak)               \n\
              \                                                            \n\
              \Leave blank for standard game style (non evaporating pawns) \n\
              \^^^^^^^^^^^         ^      ^    ^                           \n\                         
              \Enter number or leave blank and press Enter:"

-- | common bit of interface reused for every integer value to be gatehred from 
--   user input. allows for a default -> message -> minimum value -> maximum value.
getSettingSize :: Int -> String -> Int -> Int -> IO Int
getSettingSize defaulT str min max = do
  putStrLn str
  input <- getLine 
  if input == ""
    then pure defaulT
    else parseit input
  where
    parseit inp = case parse (acceptedNumber min max) "" inp of
      Left e  -> do 
        putStrLn $ last $ lines $ show e
        getSettingSize defaulT str min max
      Right boardSize -> return boardSize 

-- | to chose a different pawn  or reverse the autmatic player change after every game.
switchStartingPlayer :: StateT AppState IO ()
switchStartingPlayer = do
  state <- get
  let current = startingPawn state
  modify (\s -> state{startingPawn= otherPlayer current})

