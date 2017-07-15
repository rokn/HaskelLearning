split :: [a] -> Int -> ([a], [a])
split [] = ([], [])
split xs n = splitHelper [] xs n
  where splitHelper xs [] _ = (xs, [])
        splitHelper xs ys n | n > 0 = splitHelper (head ys : xs) (tail ys) (n-1)
                            | n == 0 = (reverse xs, ys)
