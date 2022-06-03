{-# HLINT ignore "Use list comprehension" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Eta reduce" #-}


myFilter :: Foldable t => (a -> Bool) -> t a -> [a]
myFilter fn dataSet = foldl (\acc x -> if fn x then acc else x : acc) [] dataSet

myFilter0 :: [Integer]
myFilter0 = myFilter even [1..12]

myFilter1 :: [Integer]
myFilter1 = foldl (\acc x -> if even x then acc else x : acc) [] [1..12]

fn :: [Integer] -> Integer -> [Integer]
fn = (\acc x -> if even x then acc else x : acc) 

myFilter2 :: [Integer]
myFilter2 = foldl fn [] [1..12]

myFilter3 :: Foldable t => p1 -> p2 -> [a1] -> (b -> a2 -> b) -> b -> t a2 -> b
myFilter3 f z (x:xs) = foldl

myFoldl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
myFoldl f z [] = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' fn dataSet = myFoldl (\acc x -> if fn x then acc else x : acc) [] dataSet




-- >>> myFilter' even [1..12]
-- [11,9,7,5,3,1]
-- [11,9,7,5,3,1]


