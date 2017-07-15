not' :: Bool -> Bool
not' True = False
not' False = True

and', or', nand', nor', xor', impl', equ' :: Bool -> Bool -> Bool

and' True True = True
and' _ _ = False

or' False False = False
or' _ _ = True

nand' a b = not' $ a `and'` b

nor' a b = not' $ a `or'` b

xor' False True = True
xor' True False = True
xor' _ _ = False

impl' True False = False
impl' _ _ = True

equ' a b = not' $ a `xor'` b

table :: (Bool -> Bool -> Bool) -> [(Bool,Bool,Bool)]
table f = helper f True True
  where helper f a b
          | a `and'` b = tup : helper f True False
          | not' $ a `impl'` b = tup : helper f False True
          | not' $ b `impl'` a = tup : helper f False False
          | not' $ a `or'` b = [tup]
            where tup = (a,b,f a b)

