dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n-1) xs ++ dropEvery(drop n xs) n
