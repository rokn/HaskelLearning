collatzLength :: Integer -> Integer
collatzLength 1 = 1
collatzLength n = 1 + collatzLength next
  where next = if even n then n`div`2 else 3*n + 1
