tightPrimeEmbedding :: Integer -> (Integer,Integer,Integer)
tightPrimeEmbedding i 
	| i < 2 = (0,i,0)
	| otherwise  = (smaller,i,bigger) 
	where	sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]
		primes = sieve [2..]
		smaller = last (filter (i<) primes)
		bigger = head (filter (>i) primes)

-- checksum :: Integer -> String

-- filterForFrequency :: String -> Int -> [Char]i

-- isPowerOfTwo :: String -> Bool  
