-- | module to gather functions that need customisation and are used around
--   the code base. e.g. @head@ neads edge case considerd not to raise warnings
module Helpers where

head :: [a] -> Maybe a
head []     = Nothing
head (x:xs) = Just x

