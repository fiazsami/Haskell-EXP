
------------------------------------------------------------
-- NAME  : absList
-- PARAM : a list of positive/negative integers
-- RESULT: a list of positive integers
-- >>> absList [1, -2, -5, -4, 2, 3, 4, -2]
-- [1,2,5,4,2,3,4,2]




------------------------------------------------------------
-- NAME   : countNon
-- PARAM 1: a character
-- PARAM 2: a string
-- RESULT : an integer of the sum of all characters that are NOT param 1
-- >>> countNon 'b' "abcdeabcde"
-- 8

-- >>> countNon 'a' "aaabbbccc"
-- 6



------------------------------------------------------------
-- NAME   : charChart
-- PARAM 1: a character
-- PARAM 2: a list of positive integers
-- RESULT : a list of strings where each string is of PARAM 1, of length corresponding to each number in PARAM 2
-- >>> charChart '*' [1, 2, 3, 4] 
-- ["*","**","***","****"]




------------------------------------------------------------
-- NAME   : remove
-- PARAM 1: a character
-- PARAM 2: a string
-- RESULT : a new string with PARAM 1 removed from PARAM 2
-- >>> remove 't' "this is the target"
-- "his is he arge"




------------------------------------------------------------
-- NAME   : insertBefore
-- PARAM 1: a character
-- PARAM 2: a character
-- PARAM 3: a string
-- RESULT : a new string with PARAM 2 inserted before PARAM 1 within PARAM 3
-- >>> insertBefore 't' '|' "this is the target"
-- "|this is |the |targe|t"




------------------------------------------------------------
-- NAME   : insertAfter
-- PARAM 1: a character
-- PARAM 2: a character
-- PARAM 3: a string
-- RESULT : a new string with PARAM 2 inserted after PARAM 1 within PARAM 3
-- >>> insertAfter 't' '|' "this is the target"
-- "t|his is t|he t|arget|"
