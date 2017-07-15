rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n | n > 0 = rotate (tail xs ++ [head xs]) (n-1)
            | n < 0 = rotate (init (last xs : xs)) (n+1)
