insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt y l@(x:xs) n | n == 1 = y : x : xs
                      | n < 1 || n > length l = error "Index not in range"
                      | otherwise = x : (insertAt y xs $ n-1)
