import qualified Data.List as L

data Element a = Single a | Multiple (Int, a)
  deriving(Show)

encode :: (Eq a) => [a] -> [Element a]
encode xs = map toElement (L.group xs)
  where toElement [x] = Single x
        toElement l@(x:_)= Multiple(length l, x)
