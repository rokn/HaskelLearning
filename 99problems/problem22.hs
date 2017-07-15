range :: Int -> Int -> [Int]
range x y | x < y = x : range(x+1) y
          | x > y = x : range(x-1) y
          | x == y = [x]
