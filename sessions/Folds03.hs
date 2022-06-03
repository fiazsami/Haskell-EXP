{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Eta reduce" #-}

-- LINEAR RECURSION
myFoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
myFoldr f z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFilterLin :: (a -> Bool) -> [a] -> [a]
myFilterLin fn list = myFoldr (\x acc -> if fn x then acc else x : acc) [] list

oddLin :: Integral a => [a] -> [a] -> [a]
oddLin acc [] = acc
oddLin acc (x:xs) = if even x then oddLin acc xs else x : oddLin acc xs

-- TAIL RECURSION
myFoldl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
myFoldl f z [] = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

myFilterOdd :: (a -> Bool) -> [a] -> [a]
myFilterOdd fn list = myFoldl (\acc x -> if fn x then acc else x : acc) [] list

oddTail :: Integral a => [a] -> [a] -> [a]
oddTail acc [] = acc
oddTail acc (x:xs) = if even x then oddTail acc xs else oddTail (x : acc) xs

