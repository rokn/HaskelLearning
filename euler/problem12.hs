isqrt = floor . sqrt . fromIntegral
getTrigNumbWithDivs :: Int -> Integer
getTrigNumbWithDivs n = helper 1 2 n
  where getDivCount n = length [d | d<-[1..isqrt n], (n`mod`d)==0] * 2
        helper t c n
          | getDivCount t > n = t
          | otherwise = helper (t+c) (c+1) n
