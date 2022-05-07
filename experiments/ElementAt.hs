{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

elementAt :: (Eq t, Num t) => t -> [p] -> p
elementAt i list = f 0 i list
  where
    f _ _ [] = error "Error"
    f k i (x:xs) = if k == i then x else f (k+1) i xs

elementAt' :: (Eq t, Num t) => t -> [p] -> p
elementAt' i list = f 0 i list
  where
    f _ _ [] = error "Error"
    f k i (x:xs)
      | k == i    = x
      | otherwise = f (k+1) i xs

elementAt'' :: (Eq t, Num t) => t -> [p] -> p
elementAt'' i list = f 0 i list
  where
    f _ _ [] = error "Error"
    f k i (x:xs) = case k == i of
      True -> x
      False -> f (k+1) i xs