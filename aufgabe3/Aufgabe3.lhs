> module Aufgabe3 where
> import Control.Monad

> coins = [200,100,50,20,10,5,2,1]

Returns true if list consists only of binary digits and
there are no succeeding zeros

> zeroTest :: [Integer] -> Bool
> zeroTest [] = False
> zeroTest (x:[]) = x == 1 || x == 0
> zeroTest (x:xs)	| x == 0 = 0 /= head xs && zeroTest xs
>					| x == 1 = zeroTest xs
>					| otherwise = False

Calculates the fibonacci sequence

> fib :: Integer -> Integer
> fib 0 = 0
> fib 1 = 1
> fib n = fib (n - 2) + fib (n - 1)

Calculates the count of all lists of length n that 'zeroTest' accepts

> numberOf :: Integer -> Integer
> numberOf 0 = 0
> numberOf n = fib ((abs n) + 2)

Returns the minimum amount of coins of which the sum is n

> minNumOfCoins :: Integer -> Integer
> minNumOfCoins a = calcCoins coins $ abs a 

Returns the minimum amount of coins of which the sum is n
(and coins are selected from a given list

> calcCoins :: [Integer] -> Integer -> Integer
> calcCoins [] _ = 0
> calcCoins (x:xs) y | x <= y = (y `div` x) + calcCoins xs (y `mod` x)
>                    | otherwise = calcCoins xs y

Calculates every possible combination (selected from a given list)
of coins of which the sum is n

> splits :: Integer -> [Integer] -> Integer
> splits n m   | n == 0 = 1
>              | n < 0 = 0
>              | null m && n > 0 = 0
>              | otherwise = splits n (init m) + splits (n - last m) m 

Calculates every possible combination of coins of which the sum is n                  

> numOfSplits :: Integer -> Integer 
> numOfSplits n | abs n > 500 = -1
> numOfSplits n = splits (abs n) coins

Helper function for filter, compares the sum of an integer list with n

> sumEqualTo :: Integer -> [Integer] -> Bool
> sumEqualTo n m = (sum m) == n

Returns the head of an array of arrays, if the array is empty
it returns an empty array

> head' :: [[a]] -> [a]
> head' [] = []
> head' a = head a 

Returns a single possible combination of n coins that sum up to m,
when not possible, it returns an empty list

> change :: Integer -> Integer -> [Integer]
> change n m	| abs n > 500 = []
>		| otherwise = head' $ filter (sumEqualTo (abs n)) $ replicateM (fromIntegral m) coins
