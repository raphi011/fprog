> module Aufgabe3 where


> import Control.Monad

> coins = [200,100,50,20,10,5,2,1]

> zeroTest :: [Integer] -> Bool
> zeroTest [] = False
> zeroTest (x:[]) = x == 1 || x == 0
> zeroTest (x:xs)	| x == 0 = 0 /= head xs && zeroTest xs
>					| x == 1 = zeroTest xs
>					| otherwise = False


> fib :: Integer -> Integer
> fib 0 = 0
> fib 1 = 1
> fib n = fib (n - 2) + fib (n - 1)

> numberOf :: Integer -> Integer
> numberOf 0 = 0
> numberOf n = fib ((abs n) + 2)

> minNumOfCoins :: Integer -> Integer
> minNumOfCoins a = calcCoins coins $ abs a 

> calcCoins :: [Integer] -> Integer -> Integer
> calcCoins [] _ = 0
> calcCoins (x:xs) y | x <= y = (y `div` x) + calcCoins xs (y `mod` x)
>                    | otherwise = calcCoins xs y


> splits :: Integer -> [Integer] -> Integer
> splits n m   | n == 0 = 1
>              | n < 0 = 0
>              | null m && n > 0 = 0
>              | otherwise = splits n (init m) + splits (n - last m) m 
                  

> numOfSplits :: Integer -> Integer 
> numOfSplits a | abs a > 500 = -1
> numOfSplits a = splits (abs a) coins

> sumEqualTo :: Integer -> [Integer] -> Bool
> sumEqualTo n m = (sum m) == n

> head' :: [[a]] -> [a]
> head' [] = []
> head' a = head a 

> change :: Integer -> Integer -> [Integer]
> change n m	| abs n > 500 = []
>		| otherwise = head' $ filter (sumEqualTo (abs n)) $ replicateM (fromIntegral m) coins
