isPrime :: Integer -> Bool
isPrime n | n < 2 = False
          | otherwise = not . or $ map (divisible n) [2..isqrt n]
          where divisible x y = x `rem` y == 0
                isqrt = floor . sqrt . fromIntegral


primeFactors :: Integer -> [Integer]
primeFactors n | even n = 2 : primeFactors (n`div`2)
               | otherwise = pfHelper n 3
                 where pfHelper :: Integer -> Integer -> [Integer]
                       pfHelper n i | i > isqrt(n) = if n > 2 then [n] else []
                                    | n `mod` i == 0 = i : pfHelper (n`div`i) i
                                    | otherwise = pfHelper n (i+2)
                       isqrt = floor . sqrt . fromIntegral
