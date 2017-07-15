quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
     let smallerSorted = quicksort [a | a <- xs, a <= x]
         biggerSorted = quicksort [a | a <- xs, a > x]
     in smallerSorted ++ [x] ++ biggerSorted


list :: [Integer]
list = [4,3,2,6,2,1,5,7,1,3]

main :: IO ()
main = print (quicksort list)
