import Data.Char (isNumber)

------------------------------------------------------------
-- NAME   : getMinMax
-- PARAM  : a list of integers
-- RESULT : a tuple with the minimum and maximum integers
-- >>> getMinMax [1, -2, -5, -4, 2, 3, 4, -2]
-- (-5,4)




------------------------------------------------------------
-- NAME   : sumDigits
-- PARAM  : a string
-- RESULT : an integer representing the sum of all single digits
-- >>> sumDigits "123"
-- 6
-- >>> sumDigits "123456"
-- 21




------------------------------------------------------------
-- NAME   : shiftR
-- PARAM 1: an integer
-- PARAM 2: a list
-- RESULT : a list that is shifted right the same amount as PARAM 1
-- >>> shiftR 2 [1, 2, 3, 4, 5]
-- [5,4,1,2,3]
-- >>> shiftR 5 ["a", "b", "c", "d", "e", "f", "g"]
-- ["g","f","e","d","c","a","b"]




------------------------------------------------------------
-- NAME   : amplify
-- PARAM 1: an integer
-- PARAM 2: a list
-- RESULT : a new list where each element in PARAM 2 is repeated (amplified) the same amount as PARAM 1
-- >>> amplify 3 ["a", "-", "b", "/", "c", "*"]
-- ["a","a","a","-","-","-","b","b","b","/","/","/","c","c","c","*","*","*"]
-- >>> amplify 3 [1, 2, 3]
-- [1,1,1,2,2,2,3,3,3]




------------------------------------------------------------
-- NAME   : getDigits
-- PARAM 1: a string
-- RESULT : a string with only positive digits
-- >>> getDigits "3j4j1k2k33,3j3h3b3h2h2m2k2;;2"
-- "341233333322222"




------------------------------------------------------------
-- NAME   : splice
-- PARAM 1: an integer
-- PARAM 2: an integer
-- PARAM 3: a list
-- RESULT : a list that does not have characters within PARAM 1 (inclusive) and PARAM 2 (non-inclusive)
-- >>> splice 3 6 "abcdefghijkl"
-- "abcghijkl"

-- >>> splice 4 8 [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
-- [0,1,2,3,8,9,10,11,12]
