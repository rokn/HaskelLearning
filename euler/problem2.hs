isEven :: Int -> Bool
isEven x = (mod x 2) == 0

getEvenValue :: Int -> Int
getEvenValue x | isEven x = x
               | otherwise = 0

findSum :: Int -> Int -> Int
findSum pr pr2 | pr2 >= 4000000 = 0
               | otherwise = getEvenValue pr2 + findSum pr2 (pr + pr2)

main = print(findSum 1 2)
