isqrt = floor . sqrt . fromIntegral
divisable x y = (==0) $ (x `rem` y)

isPrime :: Int -> Bool
isPrime x
  | x <= 3 = True
  | otherwise = not $ or [divisable x n | n<-[2..(isqrt x)]]

largestPrimeFactor :: Int -> Int
largestPrimeFactor x
  | x < 2 = x
  | otherwise = findPrimeFactor x $ isqrt x
   where findPrimeFactor x c
           | divisable x c = if isPrime c then c else if isPrime other then other else cont x c
           | otherwise = cont x c
           where cont x c = findPrimeFactor x $ c-1
                 other = x`div`c
