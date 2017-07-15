pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = packWith x xs 1
  where packWith :: (Eq a) => a -> [a] -> Int -> [[a]]
        packWith x [] n = [take n (repeat x)]
        packWith x (y:xs) n | x == y = packWith y xs (n+1)
                            | otherwise = [take n (repeat x)] ++ packWith y xs 1

pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 l@(x:xs) = (takeWhile (==x) l) : pack2 (dropWhile (==x) xs)
