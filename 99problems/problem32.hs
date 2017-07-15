myGcd :: Int -> Int -> Int
myGcd 0 y = abs y
myGcd x y
  | x < 0 || y < 0 = myGcd (abs x) (abs y)
  | remainder == 0 = min x y
  | otherwise = myGcd (min x y) remainder
  where remainder = (max x y) `rem` (min x y)
