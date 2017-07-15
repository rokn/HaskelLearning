myLast :: [a] -> a
myLast [] = error "can't find last of empty list"
myLast [x] = x
myLast (_:xs) = myLast xs
