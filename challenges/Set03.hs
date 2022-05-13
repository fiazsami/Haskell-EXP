
------------------------------------------------------------
-- NAME   : toCoins
-- PARAM  : an integer representing pennies
-- RESULT : a list of tuples where each element is a pair; the fist item in the tuple is the count, the second is the denomications ([25, 10, 5, 1])

-- the first parameter is $0.94
-- >>> toCoins 94
-- [(3,25),(1,10),(1,5),(4,1)]

-- the first parameter is $1.14
-- >>> toCoins 114
-- [(4,25),(1,10),(0,5),(4,1)]




------------------------------------------------------------
-- NAME   : readChange
-- PARAM  : a list of tuples where each element is a pair; the fist item in the tuple is the count, the second is the denomications
-- RESULT : a list of tuples where each element is a pair; the fist item in the tuple is the count, the second is the denomications as a string; the string should be appropriately pluralized
-- >>> readChange (toCoins 94)
-- [(3,"quarters"),(1,"dime"),(1,"nickel"),(4,"pennies")]

-- >>> readChange (toCoins 114)
-- [(4,"quarters"),(1,"dime"),(0,"nickels"),(4,"pennies")]




------------------------------------------------------------
-- NAME   : acronym
-- PARAM  : a string of words (separated by spaces)
-- RESULT : a sting with only the first letter of each word
-- >>> acronym "Central Intelligence Agency"
-- "CIA"

-- >>> acronym "Federal Bureau of Investigation"
-- "FBoI"



------------------------------------------------------------
-- NAME   : rotate
-- PARAM  : a list
-- RESULT : a list where each even index from the front is swapped with the even index from the end
-- >>> rotate [1, 2, 3, 4, 5, 6, 7]
-- [7,2,5,4,3,6,1]

-- >>> rotate ["a1", "b2", "c3", "d4", "e5", "f6", "g7"]
-- ["g7","b2","e5","d4","c3","f6","a1"]

-- >>> rotate [1, 2, 3, 4, 5, 6]
-- [6,2,4,3,5,1]

-- >>> rotate ["a1", "b2", "c3", "d4", "e5", "f6"]
-- ["f6","b2","d4","c3","e5","a1"]
