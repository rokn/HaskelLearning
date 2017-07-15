slice :: [a] -> Int -> Int -> [a]
slice [] _ _= []
slice (x:xs) n m | m <= 0 = []
                 | n <= 1 = x : slice xs 1 (m-1)
                 | otherwise = slice xs (n - 1) (m - 1)
