isListPalindrome :: Eq a => [a] -> Bool
isListPalindrome [] = True
isListPalindrome [_] = True
isListPalindrome (x:xs)
  | x == last xs = isListPalindrome $ init xs
  | otherwise = False

toList :: Integral x => x -> [x]
toList x
  | x < 10 = [x]
  | otherwise = toList (x`div`10) ++ [x`mod`10]

findPalindromic :: [Int] -> [Int]
findPalindromic xs = [n | x<-rev, y<-rev, let n = x*y, isPalindromic n]
                      where rev = reverse xs
                            isPalindromic x = isListPalindrome $ toList x
