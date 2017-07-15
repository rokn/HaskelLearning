import Data.List

divisable x y = x`mod`y == 0

findBiggestDivisor :: Integral a => [a] -> a
findBiggestDivisor xs = findBiggestDivisor' maxN xs
  where maxN = maximum xs
        findBiggestDivisor' n xs
          | listDivisable n xs = n
          | otherwise = findBiggestDivisor' (n+maxN) xs
          where listDivisable n xs = and [divisable n x | x<-xs]
