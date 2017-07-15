import Tree

areSymmetric :: Tree a -> Tree a -> Bool
areSymmetric Empty Empty = True
areSymmetric _ Empty = False
areSymmetric Empty _ = False
areSymmetric (Branch _ lx rx) (Branch _ ly ry) = areSymmetric lx ry && areSymmetric rx ly

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = areSymmetric l r
