# Notes to self.

## Study on how to get diagonals smaller than Board lenght


```
[0 0, O 1, 0 2, 0 3]  
[1 0, 1 1, 1 2, 1 3]
[2 0, 2 1, 2 2, 2 3]
[3 0, 3 1, 3 2, 3 3]

[[rows !! i !! if n > 0 then (len - i) else i, rows !! i + n !! (len - (i - n))] | i <- [0..len]]

--                         [[rows !! i      !!  if n > 0 then (len - i) else i,         <-- a side
--                           rows !! i + n !!   (len - (i - n))]                        <-- b side
--                                                              | i <- [0..len]]
--
-- n = 0
-- len = 3  -- (length - 1)
--
-- [0 0, 1 1, 2 2, 3 3]              0 1 2 3 -- 0 1 2 3                                 <-- a side
--                                   i          if n > 0 then (len - i) else i          <-- a side
-- [0 3, 1 2, 2 1, 3 0]              0 1 2 3 -- 3 2 1 0                                 <-- b side
--                                   i+n        len - (i - n)                           <-- b side
--
--
-- n = 1
-- len = 2
--
-- [0 2, 1 1, 2 0]                   0 1 2   -- 2 1 0                                   <-- a side
--                                   i          if n > 0 then (len - i) else i          <-- a side
-- [1 3, 2 2, 3 1]                   1 2 3   -- 3 2 1                                   <-- b side
--                                   i+n        len - (i - n)                           <-- b side
--
--
-- n = 2
-- len = 1
--
-- [0 1, 1 0]                        0 1     -- 1 0                                     <-- a side
--                                   i          if n > 0 then (len - i) else i          <-- a side 
-- [2 3, 3 2]                        2 3     -- 3 2                                     <-- b side
--                                   i+n        len - (i - n)                           <-- b side
--
--
-- n = 3
-- len = 0
--
-- [0 0]                             0       -- 0                                       <-- a side
--                                   i          if n > 0 then (len - i) else i          <-- a side
-- [3 3]                             3       -- 3                                       <-- b side
--                                   i+n        len - (i - n)                           <-- b side
```
