{-# LANGUAGE OverloadedStrings #-}
-- | Module with all the parsers used to read Tasks from file and validate CLI inputs.
module Parser where

import Text.Parsec 
  ( digit, many1, (<|>), try, eof
  , noneOf, notFollowedBy, string
  , count, oneOf, many, letter, char)
import Text.Parsec.String (Parser)
import Data.Time (addDays, fromGregorian, Day)

import Task (Task(..), Priority)

-- | parserCheck given a string.row from the todo.txt file converts it into Task type.
-- example:
-- >>> parse parserTask "" "task-name,False,description of the task\n"
parserTask :: Parser Task
parserTask = do
  mark <- parserCompleted
  char ','
  name <- parserTaskName
  char ','
  priority <- parserPriority
  char ','
  date <- parserDate
  char ','
  description <- parserDescription
  char '\n'
  notFollowedBy eof
  return (Task mark name priority date description)

nameParserError :: String
nameParserError = "Only letters, `-`, `.` and spaces are accepted for names."

-- | parserTaskName checks validity of Task name. 
--   Allows for empty name for now as it comes handy with CLI when nochange is wanted,  
--   should be changed in the future.
parserTaskName :: Parser String
parserTaskName = do
  name <- many (letter <|> digit <|> oneOf "-. ") -- <?> "letter, `-`, `.` or space"
  notFollowedBy (noneOf ",") <|> eof -- <?> "end of input or a comma"
  return name

parserDescription :: Parser String
parserDescription = do
  description <- many (noneOf "\n")
  return description

parserCompleted :: Parser Bool
parserCompleted = do
  mark <- string "True" <|> string "False"
  notFollowedBy (noneOf ",") <|> eof
  return $ read mark

-- | Date parser (YYYY-MM-DD)
parserDate :: Parser Day
parserDate = do
  year  <- count 4 digit
  oneOf "-/. "
  month <- count 2 digit
  oneOf "-/. "
  day   <- count 2 digit
  return $ fromGregorian (read year) (read month) (read day)

parserPriority :: Parser Priority
parserPriority = do
  priority <- string "High" <|> string "Medium" <|> string "Low"
  notFollowedBy (noneOf ",") <|> eof
  return $ read priority

-- | Parses a due date from either a date string in @YYYY-MM-DD@ format or an integer string 
-- representing days to add to the current date. Returns the resulting 'Day'.
-- The first argument, of type 'Day', is the current date, typically obtained via 'Interface.processCurrentDay'.
-- Example:
-- >>> parse (parserDueDate (fromGregorian 2023 10 1)) "" "2023-10-15"
-- Right 2023-10-15
-- >>> parse (parserDueDate (fromGregorian 2023 10 1)) "" "7"
-- Right 2023-10-08
parserDueDate :: Day -> Parser Day
parserDueDate d = do
  input <- try parserDate <|> (parserDueDateFromInt d) 
  return input

-- | @parserDueDateFromInt@ is used within the @parserDueDate@ and the sole 
--   scope is to make the @Integer@ Parser with the same type as the 
--   @Parser Day@ from the date parser @parserDate@ so that we can use @(<|>)@
parserDueDateFromInt :: Day -> Parser Day
parserDueDateFromInt d = do
  num <- many1 digit
  return $ addDays (read num) d
