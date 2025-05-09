module Interface where

import System.IO (hFlush, stdout)
import Control.Monad.State
import Control.Monad (when)
-- import Control.Monad.IO.Class
import Data.Maybe (isNothing, fromMaybe)
import Text.Parsec (parse)

import Game
import Board
import Graphics
import Parser
import Ai

data AppState = AppState
  { game      :: Game
  , isLooping :: Bool
  , scoresO   :: Int
  , scoresX   :: Int
  , aiX       :: Bool
  , aiO       :: Bool
  , startingPawn :: Player
  } -- deriving (Show)

multiplayer :: AppState -> IO (AppState)
multiplayer as = do
  putStrLn "Player X should be AI or Human:"
  putStrLn "(`ai`, `human`, press enter for default (AI)"
  inputX <- getLine
  putStrLn "Player O should be AI or Human:"
  putStrLn "(`ai`, `human`, press enter for default (AI)"
  inputO <- getLine
  case input of
    "human" -> return as{humanVsAI=False} 
    "ai"    -> return as{humanVsAI=True}
    otherwise -> do
      putStrLn "please enter only `human` or `ai`"
      multiplayer as
  where
    setAi X state = undefined 
    setAi O state = undefined 

loop :: StateT AppState IO ()
loop = do
  state <- get
  printLn "-------------------------------------------------------------------\n\ 
          \COMMAND   |                                             DESCRIPTION\n\
          \----------|--------------------------------------------------------\n\
          \0         |                                 Quick Standard 3x3 Game\n\
          \1         |                              Quick Evaporating 3x3 Game\n\
          \2         |                   Stadard/Evaporating Custom Game Setup\n\
          \3         |                                        repeat last game\n\
          \          |                                                        \n\
          \repeat    |                                        repeat last game\n\
          \scores    |                  Shows the scores of all games this far\n\
          \starting  |                     Switch the player that starts first\n\
          \reset     |                  Reinitiate App (select Multiplayer/AI)\n\
          \exit      |                                       Exit from the App\n\
          \-------------------------------------------------------------------"
  printLn ""
  printLn $ "Player to start next: " ++ (show $ startingPawn state)
  printLn ""
  printLn "Please, enter a command from the list above:"
  input <- getLn 
  handleInput input
  state <- get
  when (isLooping state) loop

handleInput :: String -> StateT AppState IO ()
handleInput "exit" = do
  printLn "Goodbye!"
  modify (\s -> s {isLooping = False})
--
handleInput "0" = do
  printLn "Quick Standard Game Started!"
  setStartintPawn
  gameLoop
  endGame 
-- 
handleInput "1" = do
  printLn "Quick Evaporating Game Started!"
  modify (\s -> s{game=(game s){evaporCount= Just 3}})
  setStartintPawn
  gameLoop
  endGame 
-- 
handleInput "2" = do
  printLn ""
  buildCustomGame
  setStartintPawn
  gameLoop
  endGame
--
handleInput "3" = do
  state <- get
  printLn ""
  printLn "Replaying game with:"
  printLn $ "Evaporating value: " ++ (show $ evaporCount $ game state)
  printLn $ "Winning strake value: " ++ (show $ countToWin $ game state)
  setStartintPawn
  gameLoop
  endGame
--
handleInput "repeat" = handleInput "3"
--  
handleInput "starting" = do
  switchStartingPlayer
  printLn "Switched starting player"
-- 
handleInput "scores" = do
  state <- get
  printLn ""
  printLn "Scores are calculated by assigning the winning streak number for each win."
  printLn ""
  printLn $ "Player X scores: " ++ (show $ scoresX state) 
  printLn $ "Player O scores: " ++ (show $ scoresO state) 
  printLn ""
  printLn "press Enter to continue."
  input <- getLn
  printLn ""
-- 
handleInput "reset" = do
  let app = AppState (mkGame 3 3) True 0 0 True X
  printLn "Games history reset."
  app' <- liftIO $ multiplayer app
  modify $ const app'
-- 
handleInput input = do
  printLn $ "You entered: " ++ input
  printLn $ "err: `" ++ input ++ "` is not a valid command."
-- handleInput end


setStartintPawn :: StateT AppState IO ()
setStartintPawn = do
  state <- get
  let gm = game state
  let lastP = otherPlayer $ startingPawn state
  let gm' = gm{lastPlayer= lastP}
  modify (\s -> s{game= gm'})

gameLoop :: StateT AppState IO ()
gameLoop = do
  state <- get
  displayGame state 
  let player = otherPlayer $ lastPlayer $ game state
  if humanVsAI state 
    then getAiMove $ game state
    else getPlayerCoordinates player state 
  state' <- get
  when ((isNothing $ win $ game state') && (not $ end $ game state')) gameLoop
  handleInput "starting"

getAiMove :: Game -> StateT AppState IO ()
getAiMove gm = modify (\s -> s{game= newGame})
  where 
    newGame = makeAiMove gm

getPlayerCoordinates :: Player -> AppState -> StateT AppState IO ()
getPlayerCoordinates p state = do 
  printLn $ "Enter coordinates for player `" ++ show p ++ "` next move. (e.g. a1, A1, a 1, 1 1)"
  input <- getLn
  let letterIndexes = take (length $ board $ game state) $ charIndexes8 charsIxs 
  case parse (coordinateParser letterIndexes) "" input of
    Left e   -> printLn "You entered an invlaid coordinate format,\n\
                        \please stick to this patter examples:\n\
                        \`a1`, `A1`, `a 1`, `A 1`, `1 1`."
    Right xy -> case move xy $ game state of
      Left e   -> printLn e
      Right gm -> modify (\s -> s {game=gm})

switchStartingPlayer :: StateT AppState IO ()
switchStartingPlayer = do
  state <- get
  let current = startingPawn state
  modify (\s -> state{startingPawn= otherPlayer current})

displayGame :: AppState -> StateT AppState IO ()
displayGame state = do
  printLn "\\/"
  printLn ""
  liftIO $ printBoard $ board $ game state
  printLn ""
  printLn ""

endGame :: StateT AppState IO ()
endGame = do
  state <- get
  displayGame state
  let winner = win $ game state 
  if isNothing winner 
    then printLn "The game was a Draw!"
    else do
      modify $ dispatchScores (fromMaybe X winner)
      printLn $ "Winner is: " ++ (show $ fromMaybe O winner)
  clearBoard

clearBoard :: StateT AppState IO ()
clearBoard = do
  state <- get
  let gm = game state
  let boardSize = length $ board gm
  modify (\s -> s {game= gm {board= mkBoard boardSize}})

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
      msgEC = "Enter how many moves before the pawns start to evaporate.   \n\
              \It needs to be more than the winning streak.                \n\
              \Leave blank for standard game style (non evaporating pawns) \n\
              \Enter number:"

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

dispatchScores :: Player -> AppState -> AppState
dispatchScores p as 
  | p == O    = as{scoresO= pOs}
  | otherwise = as{scoresX= pXs}
  where
    ctw = countToWin $ game as
    pOs = scoresO as
    pXs = scoresX as

-- | helper function to avoid using liftIO everytime I print something inside a StateT function.
printLn :: String -> StateT AppState IO ()
printLn str = do 
  liftIO $ putStrLn str

getLn :: StateT AppState IO String
getLn = do 
  input <- liftIO $ getLine
  return input

