import qualified Data.List as L

data Element a = Single a | Multiple (Int, a)
  deriving(Show)

encode :: (Eq a) => [a] -> [Element a]
encode xs = map toElement (L.group xs)
  where toElement [x] = Single x
        toElement l@(x:_)= Multiple(length l, x)

decode :: (Eq a) => [Element a] -> [a]
decode [] = []
decode (x:xs) = case x of
                  Single a -> a : decode xs
                  Multiple (n,a) -> replicate n a ++ decode xs

decode2 :: [Element a] -> [a]
decode2 = concatMap decoder
  where decoder (Single x) = [x]
        decoder (Multiple (n,x)) = replicate n x
