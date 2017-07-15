import Tree

tree = Branch 'a' (Branch 'b' (leaf 'd') (leaf 'e'))
                  (Branch 'c' Empty (Branch 'f' (leaf 'g') Empty))

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [leaf 'x']
cbalTree 2 = [(Branch 'x' (leaf 'x') Empty), (Branch 'x' Empty (leaf 'x'))]
cbalTree n
  | even (n-1) = branchBuilder branch branch
  | otherwise = branchBuilder (cbalTree $ half+1) branch ++ branchBuilder branch (cbalTree $ half+1)

    where half = (n-1) `div` 2
          branch = cbalTree half
          branchBuilder l r = [(Branch 'x' x y) | x<-l, y<-r]

-- Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
-- Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)
-- Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty)
-- Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))

