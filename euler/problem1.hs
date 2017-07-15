isMultiple :: Integer -> Integer -> Bool
isMultiple x y = (x `mod` y) == 0

isMultipleOf3or5 :: Integer -> Bool
isMultipleOf3or5 x = (isMultiple x 3) || (isMultiple x 5)

getNumber3or5Value :: Integer -> Integer
getNumber3or5Value x
  | isMultipleOf3or5 x = x
  | otherwise = 0

sumOf3or5Multiples :: [Integer] -> Integer
sumOf3or5Multiples [] = 0
sumOf3or5Multiples (x:xs) = getNumber3or5Value x + sumOf3or5Multiples xs

main = print(show (sumOf3or5Multiples [1..999]))
