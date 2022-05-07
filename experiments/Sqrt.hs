square :: Num a => a -> a
square x = x * x

tolerance :: Double
tolerance = 0.000001

isClose :: Double -> Double -> Bool
isClose x g = tolerance > abs (x - square g)

updateGuess :: Fractional a => a -> a -> a
updateGuess x g = (g + (x / g)) / 2

mySqrt' :: Double -> Double -> Double
mySqrt' x g = if isClose x g then g else mySqrt' x (updateGuess x g)

mySqrt :: Double -> Double
mySqrt x = mySqrt' x 1

-- >>> sqrt 3
-- 1.7320508075688772

-- >>> mySqrt 3
-- 1.7320508100147274
