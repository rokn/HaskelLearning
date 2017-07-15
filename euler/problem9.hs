isqrt = floor . sqrt . fromIntegral

-- isPrime :: Integer -> Bool
-- isPrime x = null [c | c<-2:3:[6,12..isqrt(x)], divisable x c]
  -- where divisable x y = (x`mod`y)==0
  --       -- isPrime' _ 1 = True
        -- isPrime' x c
        --   | divisable x c = False
        --   | otherwise = isPrime' x (c-1)
        --
getPrimes :: [Integer] -> [Integer]
getPrimes [] = []
getPrimes (x:xs) = x:(getPrimes $ filter(not . flip divisable x) xs)
  where divisable x y = (x`mod`y)==0

isPrime::Integer->Bool
isPrime n =
  let
  sqrt' = truncate $ sqrt (fromIntegral n)
  test [] = True
  test (x:xs) =
    if mod n x == 0
       then False
       else test xs
    in
    test [2..sqrt']

-- nextPrime :: Integer -> Integer
-- nextPrime 2 = 3
-- nextPrime x = head $ dropWhile (not . isPrime) [x+2,x+4..]

-- getPrimesLowerThan :: Integer -> [Integer]
-- getPrimesLowerThan n = getPrimesLowerThan' [nextPrime 2] n
--   where getPrimesLowerThan' l@(_:xs) n
--           | curr >= n = xs
--           | otherwise = getPrimesLowerThan' ((nextPrime curr):l) n
--             where curr = head l
