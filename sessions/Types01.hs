newtype RGBInt = RGBInt Int deriving (Show)

data Color = Color RGBInt RGBInt RGBInt deriving (Show)

toColor :: Int -> Int -> Int -> Color
toColor r g b = Color (toRGBInt r) (toRGBInt g) (toRGBInt b)

toRGBInt :: Int -> RGBInt
toRGBInt i
    | -1 < i && i < 256 = RGBInt i
    | otherwise = error "out of bounds"


-- >>> toColor 0 1 256
-- out of bounds
