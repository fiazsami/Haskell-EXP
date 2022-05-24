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
myFilter0 = myFilter even [1..50]

myFilter1 :: [Integer]
myFilter1 = foldr (\x acc -> if even x then acc else x : acc) [] [1..50]

fn :: Integer -> [Integer] -> [Integer]
fn = (\x acc -> if even x then acc else x : acc)

myFilter2 :: [Integer]
myFilter2 = foldr fn [] [1..50]

myFoldr f z [] = z
myFoldr f z (x:xs) = foldr