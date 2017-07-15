isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome l@(x:xs) | x == last xs = isPalindrome (init xs)
                      | otherwise = False
