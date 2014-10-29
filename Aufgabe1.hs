import Data.List

sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]
primes = sieve [2..] 

{- tightPrimeEmbedding
 - takes an integer and embeds it in a 3-tuple with 
 - the next lower or equal and higher prime number
 -} 
tightPrimeEmbedding :: Integer -> (Integer,Integer,Integer)
tightPrimeEmbedding i | i < 2 = (0,i,0) 
                      | otherwise = go primes 
 where go (x:y:xs) = 	if x <= i && y > i
			then (x,i,y)
			else go (y:xs)

{- checksum 
 - calculates the digit sum of an integer
 - and returns the binary representation as a string
 -} 
checksum :: Integer -> String
checksum 0 = "0"
checksum i = binRep (crossSum i)
	where	binRep 0 = ""
		binRep n
			| even n = binRep ( n `div` 2) ++ "0"
			| odd n = binRep ( n `div` 2) ++ "1"
		crossSum 0 = 0 
		crossSum x = (x `mod` 10) + crossSum (x `div` 10)  

{- removeDuplicates
 - removes duplicate characters from the string
 - and preserves the order
 -} 
removeDuplicates :: String -> [Char]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : (removeDuplicates $ filter (/= x) xs)  

{- frequency 
 - Returns a list that contains only elements 
 - that contains chars that appear n times
 -}
frequency :: String -> Int -> [Char]
frequency s n = [ x | x <- s, occurence s x == abs n] 

{- occurence
 - counts the occurence of a character in a string
 -} 
occurence :: String -> Char -> Int
occurence s c = length $ filter (== c) s


{- filterForFrequency
 - Returns a list of all characters, which
 - appear n times
-}
filterForFrequency :: String -> Int -> [Char]
filterForFrequency [] _ = []
filterForFrequency s n  = removeDuplicates $ frequency s n 


{- isPowerOfTwo
 - Returns true if the product of count of each lower/uppercase vowel
 - is a number with the power of two
 -} 
isPowerOfTwo :: String -> Bool
isPowerOfTwo s = power tuples	
	where	vocals = filter (`elem` ['a','e','i','o','u','A','E','I','O','U']) s
		tuples = product $ map (\x -> length x) $ group $ sort vocals
		power n | n == 1 = True
			| n > 1 && n `mod` 2 == 0 = power ( n `div` 2)
			| otherwise = False

