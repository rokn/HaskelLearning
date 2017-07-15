import qualified Data.List as L

combinations :: Int -> [a] -> [[a]]
combinations n l@(x:xs) | n == length l = [l]
                        | n <= 0 = [[]]
                        | n == 1 = map (\x -> [x]) l
                        | n > length l = error "can't combinate list with smaller size"
                        | otherwise = (map ((:)x) (combinations (n-1) xs)) ++ combinations n xs

myGroup :: (Eq a) => [Int] -> [a] -> [[[a]]]
myGroup (c:comb) xs = grouper comb xs $ map (\x -> [x]) (combinations c xs)
  where grouper [] _ curr = curr
        grouper (c:comb) xs curr = grouper comb xs (addLevel c xs curr)
        addLevel n xs curr = concatMap (\x ->map (\y -> x ++ [y]) (combinations n (xs L.\\ concat x))) curr
