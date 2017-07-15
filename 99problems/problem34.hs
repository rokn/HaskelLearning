totient :: Int -> Int
totient 1 = 1
totient n = length $ filter ((==)1 . gcd n) [1..n-1]
