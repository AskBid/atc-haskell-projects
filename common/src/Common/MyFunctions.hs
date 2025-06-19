module Common.MyFunctions where

headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe (a:as) = Just a
