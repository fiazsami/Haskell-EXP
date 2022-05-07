{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

myLast :: [p] -> p
myLast [] = error "No elements"
myLast [x] = x
myLast (x:xs) = myLast xs


myButLast :: [p] -> p
myButLast [] = error "No elements"
myButLast [x] = error "Requires more elements"
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs


elementAt :: (Eq t, Num t) => t -> [p] -> p
elementAt _ [] = error "Not found"
elementAt i (x:xs) = if i == 1 then x else elementAt (i - 1) xs


myLength :: Num p => [a] -> p
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


isPalindrome :: Eq a => [a] -> Bool
isPalindrome str = str == myReverse str



