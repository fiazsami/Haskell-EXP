newton :: Fractional a => a -> a -> a
newton x g = (g + (x / g)) / 2

-- >>> newton 3 1.7320508100147274
-- >>> sqrt 3
-- 1.7320508075688772
-- 1.7320508075688772

square :: Num a => a -> a
square x = x * x

-- >>> square 1.7320508100147274
-- 3.0000000084726732

isGoodEnough :: (Ord a, Fractional a) => a -> a -> Bool
isGoodEnough x g = abs (x - square g) < 0.0000000001

-- >>> isGoodEnough 3 1.7320508075688772
-- True

mySqrt' :: (Ord t, Fractional t) => t -> t -> t
mySqrt' x g = if isGoodEnough x g then g else mySqrt' x (newton x g)

mySqrt :: (Ord t, Fractional t) => t -> t
mySqrt x = mySqrt' x 1

-- >>> mySqrt 8
-- >>> sqrt 8
-- 2.82842712474619
-- 2.8284271247461903
