# Notes to self.

## Study on how to get diagonals smaller than Board lenght


```
[0 0, O 1, 0 2]
[1 0, 1 1, 1 2]
[2 0, 2 1, 2 2]

-- calculating smaller diagonals as well.
-- [0 0, 1 1, 2 2]
-- [0 2, 1 1, 2 0]
-- [0 1, 1 0] 
-- [1 2, 2 1]
-- [0 0]
-- [2 2]

[0 0, O 1, 0 2, 0 3]
[1 0, 1 1, 1 2, 1 3]
[2 0, 2 1, 2 2, 2 3]
[3 0, 3 1, 3 2, 3 3]

-- len = 3 (length - 1)
-- [0 0, 1 1, 2 2, 3 3]              0 1 2 3 -- 0 1 2 3
-- i=[0..len]                        i          i
-- [0 3, 1 2, 2 1, 3 0]              0 1 2 3 -- 3 2 1 0 
--                                   i+0        len - (i - 0)
--
-- len = 2 (length - 1)
-- [0 2, 1 1, 2 0]                   0 1 2   -- 2 1 0
-- i=[0..len-1]                      i          len - i  
-- [1 3, 2 2, 3 1]                   1 2 3   -- 3 2 1 
--                                   i+1        len - (i - 1)
--
-- len = 1 (length - 1)
-- [0 1, 1 0]                        0 1     -- 1 0
-- i=[0..len-2]                      i          len - i       
-- [2 3, 3 2]                        2 3     -- 3 2
--                                   i+2        len - (i - 2)
-- [0 0]
-- [3 3]
--
```
