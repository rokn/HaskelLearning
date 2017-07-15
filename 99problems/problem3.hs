elementAt :: [a] -> Int -> a
elementAt [] _ = error "Can't find element in empty"
elementAt l@(x:xs) n | n == 1 = x
                     | n > length l || n < 1 = error "Index out of range"
                     | otherwise = elementAt xs (n-1)
