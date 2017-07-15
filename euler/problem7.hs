isqrt = floor . sqrt . fromIntegral

nextPrime :: Integer -> Integer
nextPrime 2 = 3
nextPrime x = head $ dropWhile (not . isPrime) [x+2,x+4..]
  where isPrime x = isPrime' x $ isqrt x
        divisable x y = (x`mod`y)==0
        isPrime' _ 1 = True
        isPrime' x c
          | divisable x c = False
          | otherwise = isPrime' x (c-1)

findNPrime :: Integer -> Integer
findNPrime 1 = 2
findNPrime n = nextPrime $ findNPrime $ n-1
