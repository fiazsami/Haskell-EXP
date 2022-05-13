{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}


countItems :: Num t => t -> [a] -> t
countItems count [] = count
countItems count (x:xs) = countItems (count+1) xs

tailFactorial :: Num t => t -> [t] -> t
tailFactorial answer [] = answer
tailFactorial answer (x:xs) = tailFactorial (answer * x) xs


countOcc :: Num t => Char -> [Char] -> t -> t
countOcc _ "" count = count
countOcc char (x:xs) count = 
    if char == x 
    then countOcc char xs (count+1)
    else countOcc char xs count


replaceLinear :: Eq a => a -> a -> [a] -> [a]
replaceLinear _ _ [] = []
replaceLinear t r (x:xs) = 
    if x == t
    then r : replaceLinear t r xs
    else x : replaceLinear t r xs

replaceTail :: Eq a => a -> a -> [a] -> [a] -> [a]
replaceTail _ _ [] result = reverse result
replaceTail t r (x:xs) result = 
    if x == t
    then replaceTail t r xs (r : result)
    else replaceTail t r xs (x : result)




