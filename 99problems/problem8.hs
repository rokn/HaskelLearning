compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compressWith x xs
  where compressWith _ [] = []
        compressWith x (y:xs) | x == y = compressWith y xs
                              | length xs == 0 = []
                              | otherwise = y : compressWith y xs
