{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}

myFilter :: Foldable t => (a -> Bool) -> t a -> [a]
myFilter fn dataSet = foldr (\x acc -> if fn x then acc else x : acc) [] dataSet

myFilter0 :: [Integer]
myFilter0 = myFilter even [1..12]

myFilter1 :: [Integer]
myFilter1 = foldr (\x acc -> if even x then acc else x : acc) [] [1..12]

fn :: Integer -> [Integer] -> [Integer]
fn = (\x acc -> if even x then acc else x : acc)

myFilter2 :: [Integer]
myFilter2 = foldr fn [] [1..12]

myFilter3 :: Foldable t => p1 -> p2 -> [a1] -> (a2 -> b -> b) -> b -> t a2 -> b
myFilter3 f z (x:xs) = foldr

myFoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
myFoldr f z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' fn dataList = myFoldr (\x acc -> if fn x then acc else x : acc) [] dataList
