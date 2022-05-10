
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Use odd" #-}

import System.IO ()


add2Pos :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2
add2Pos x y = if y == 0 then x else add2Pos (x+1) (y-1)

trunc :: [Char] -> [Char]
trunc table = "truncate " ++ table

truncStmts :: [[Char]] -> [[Char]] -> [[Char]]
truncStmts res [] = res
truncStmts res (x:xs) = truncStmts (trunc x:res) xs


-- >>> truncStmts [] ["table1", "table2", "table3"]
-- ["truncate table3","truncate table2","truncate table1"]


countEven :: (Integral a, Num t) => t -> [a] -> t
countEven count [] = count
countEven count (x:xs) = 
    if x `mod` 2 == 0 
    then countEven (count+1) xs 
    else countEven count xs

countOdd :: (Integral a, Num t) => t -> [a] -> t
countOdd count [] = count
countOdd count (x:xs) =
    if x `mod` 2 /= 0
    then countOdd (count+1) xs
    else countOdd count xs


