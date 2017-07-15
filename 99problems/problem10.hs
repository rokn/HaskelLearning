pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack l@(x:xs) = (takeWhile (==x) l) : pack (dropWhile (==x) xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x))(pack xs)
