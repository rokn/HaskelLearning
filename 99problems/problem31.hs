isPrime :: Integer -> Bool
isPrime n | n < 2 = False
          | otherwise = not . or $ map (divisible n) [2..isqrt n]
          where divisible x y = x `rem` y == 0
                isqrt = floor . sqrt . fromIntegral
