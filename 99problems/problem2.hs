myButLast :: [a] -> a
myButLast [] = error "Can't find but last on empty"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs
