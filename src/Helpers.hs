-- | module to gather functions that need customisation and are used all around
--   the code base. e.g. @head@ neads edge case considerd not to raise warnings
module Helpers where

head :: [a] -> Maybe a
head []     = Nothing
head (x:xs) = Just x

(><) :: [[a]] -> [[b]] -> [[(a,b)]]
(><) [] _ = []
(><) _ [] = []
(><) (a:as) (b:bs) = zip a b : (as >< bs) 
