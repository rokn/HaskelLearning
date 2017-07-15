combinations :: Int -> [a] -> [[a]]
combinations n l@(x:xs) | n == length l = [l]
                        | n <= 0 = [[]]
                        | n == 1 = map (\x -> [x]) l
                        | n > length l = error "can't combinate list with smaller size"
                        | otherwise = (map ((:)x) (combinations (n-1) xs)) ++ combinations n xs
