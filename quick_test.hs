myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Can't find element in empty list"
elementAt (x:xs) n
  | length xs < n-1 || n <= 0  = error "Index out of range"
  | n == 1 = x
  | otherwise = elementAt xs (n-1)

myLength :: [a] -> Int
myLength x = lengthHelper x 0
  where lengthHelper [] n = n
        lengthHelper (_:xs) n = lengthHelper xs (n+1)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = palindromeCheck [] l
  where palindromeCheck l1 l2@(x:xs) | same_length = l1 == (reverse l2)
                                     | otherwise = palindromeCheck (l1 ++ [x]) xs
                                        where same_length = if even (length l1 + length l2)
                                                             then length l1 == length l2
                                                             else length l1 == (length l2 - 1)

data NestedList a = Elem a | List [NestedList a]

-- flatten :: NestedList a -> [a]
-- flatten (Elem x) = [x]
-- flatten (List (x:xs)) = flatten x ++ flatHelper xs
--   where flatHelper (x:xs) = (flatten x):(flatHelper xs)

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:ys@(y:xs))
  | x == y = compress ys
  | otherwise = x:compress ys

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack l@(x:xs) = (takeWhile (==x) l) : pack (dropWhile (==x) xs)

encode :: Eq a => [a] -> [(Int, a)]
encode = map toTuple . pack
  where toTuple l = (length l, head l)


data Element a = Single a | Multiple Int a
  deriving(Show)

encodeMod :: Eq a => [a] -> [Element a]
encodeMod = map toTuple . pack
  where toTuple l@(x:_) | len > 1 = Multiple len x
                        | otherwise = Single x
                          where len = length l

decodeMod :: [Element a] -> [a]
decodeMod [] = []
decodeMod ((Single x):xs) = x:decodeMod xs
decodeMod ((Multiple n x):xs) = (take n $ repeat x) ++ decodeMod xs

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (take n $ repeat x) ++ repli xs n
