import qualified Data.List as L
import Data.Function

lfsort :: [[a]] -> [[a]]
lfsort = concat . L.sortBy compLength . L.groupBy ((==) `on` length) . L.sortBy compLength
  where compLength = compare `on` length
