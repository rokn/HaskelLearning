import Data.List

toList :: Integer -> [Integer]
toList x
  | x < 10 = [x]
  | otherwise = toList (x`div`10) ++ [x`mod`10]

partitions :: [a] -> Int -> [[a]]
partitions l@(_:xs) n
  | length l < n = []
  | otherwise = (take n l):partitions xs n

findBigNearProduct :: Integer -> Int -> Integer
findBigNearProduct n c = maximum $ map product (partitions (toList n) c)

