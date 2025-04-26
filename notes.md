# Notes to self.

## Study on how to get diagonals smaller than Board lenght


```
[0 0, O 1, 0 2, 0 3]  
[1 0, 1 1, 1 2, 1 3]
[2 0, 2 1, 2 2, 2 3]
[3 0, 3 1, 3 2, 3 3]

                           [[rows !! i !! i, rows !! i !! (len - i)] | i <- [0..len]] :

                           [[rows !! i       !! i,                                      <-- a side
-- n = 0                     rows !! i !!       (len - (i - n))] | i <- [0..len]]       <-- b side
-- len = 3  -- (length - 1)
-- [0 0, 1 1, 2 2, 3 3]              0 1 2 3 -- 0 1 2 3                                 <-- a side
--                                   i          i                                       <-- a side
-- [0 3, 1 2, 2 1, 3 0]              0 1 2 3 -- 3 2 1 0                                 <-- b side
--                                   i+n        len - (i - n)                           <-- b side
--
-- n = 1
-- len = 2
-- [0 2, 1 1, 2 0]                   0 1 2   -- 2 1 0                                   <-- a side
--                                   i          len - i                                 <-- a side
-- [1 3, 2 2, 3 1]                   1 2 3   -- 3 2 1                                   <-- b side
--                                   i+n        len - (i - n)                           <-- b side
--
-- n = 2
-- len = 1
-- [0 1, 1 0]                        0 1     -- 1 0
--                                   i          len - i     
-- [2 3, 3 2]                        2 3     -- 3 2
--                                   i+n        len - (i - n)
--
-- n = 3
-- len = 0
-- [0 0]                             0       -- 0
--                                   i          len - i
-- [3 3]                             3       -- 3
--                                   i+n        len - (i - n)
```
