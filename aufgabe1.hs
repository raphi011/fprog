sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]
primes = sieve [2..] 

tightPrimeEmbedding :: Integer -> (Integer,Integer,Integer)
tightPrimeEmbedding i | i < 2 = (0,i,0) 
                      | otherwise = go primes 
 where go (x:y:xs) = 	if x <= i && y > i
			then (x,i,y)
			else go (y:xs)

checksum :: Integer -> String
checksum 0 = "0"
checksum i = binRep (crossSum i)
	where	binRep 0 = ""
		binRep n
			| even n = binRep ( n `div` 2) ++ "0"
			| odd n = binRep ( n `div` 2) ++ "1"
		crossSum 0 = 0 
		crossSum x = (x `mod` 10) + crossSum (x `div` 10)  

filterForFrequency :: String -> Int -> [Char]
filterForFrequency s _ = []
filterForFrequency s n  = [ x | x <- s, (occurence x) `==` (abs n)] 
	where occurence c = length $ filter ((==) c) s
		

-- isPowerOfTwo :: String -> Bool  
