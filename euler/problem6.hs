findDiff :: Floating a => [a] -> a
findDiff xs = ((sum xs)**2) - (sum([x**2|x<-xs]))
