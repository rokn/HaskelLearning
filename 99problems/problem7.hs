data NestedList a = Elem a | List [NestedList a]
  deriving(Show)

flatten :: NestedList a -> [a]
flatten xs = case xs of
               Elem a -> [a]
               List a -> if(length a == 0) then [] else flatten(head a) ++ flatten (List(tail a))

flatten' :: NestedList a -> [a]
flatten' (Elem a) = [a]
flatten' (List []) = []
flatten' (List (x:xs)) = flatten x ++ flatten (List xs)
