{-# LANGUAGE OverloadedStrings #-}

-- | This module is used to visualise boards and games statics
module Graphics where

import Board

--    -------------
--    |   |   |   |
--    -------------
--    | O |   | X |
--    -------------
--    |   |   |   |
--    -------------

lineH :: Board -> String
lineH b = line (length b)
  where
    line 0 = ""
    line count = segment ++ line (count-1)
    segment = "----"
  
