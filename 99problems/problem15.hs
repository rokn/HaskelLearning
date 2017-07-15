dupli :: [a] -> Int -> [a]
dupli [] _ = []
dupli (x:xs) n = (replicate n x) ++ dupli xs
