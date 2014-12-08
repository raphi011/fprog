module Aufgabe7 where

import Data.List
import Data.Ord

grN :: Nat -> Nat -> Bool
grN Z _ = False
grN _ Z = True 
grN (S n) (S m) = grN n m   

eqN :: Nat -> Nat -> Bool 
eqN Z Z = True
eqN Z _ = False
eqN _ Z = False 
eqN (S n) (S m) = eqN n m

maxN :: Nat -> Nat -> Nat
maxN n1 n2 | grN n1 n2 = n1
           | otherwise = n2

toInt :: Nat -> Integer
toInt Z = 0
toInt (S n) = 1 + toInt n

data Verein       = Sturm | WAC | Austria | WrNeustadt | RBSbg | Groedig | Rapid | Admira | Ried | Altach deriving (Eq,Ord,Show)

newtype SpielerId = SId Nat deriving (Eq,Ord,Show)
newtype TrainerId = TId Nat deriving (Eq,Ord,Show)

newtype Kader     = Kd (Verein -> [SpielerId]) 
newtype Trainer   = Tr (Verein -> TrainerId)

type Punkte       = (Verein -> Nat) 

type Saison       = (Punkte,Kader,Trainer)

type Historie     = [Saison]

data Nat          = Z | S Nat deriving (Eq,Ord,Show)

vereine = [Sturm, WAC, Austria, WrNeustadt, RBSbg, Groedig, Rapid, Admira, Ried, Altach]





--getBestKader :: Saison -> [SpielerId] 
--getBestKader (punkte ,Kd kader, _) = kader (getBestVerein punkte)

--getBestVerein :: Punkte -> Verein
--getBestVerein p = foldl (\cur x -> compareVereine p cur x) Sturm vereine

--getSpieler :: Kader -> Verein -> [SpielerId] 
--getSpieler (Kd k) v = k v 

--frequency :: Ord a => [a] -> [(Int,a)] 
--frequency list = map (\l -> (length l, head l)) (group (sort list))

sortRanks :: (Verein, Integer) -> (Verein, Integer) -> Ordering
sortRanks (v1, p1) (v2, p2) | p1 > p2 = GT
                            | p1 < p2 = EQ
                            | otherwise = compare v1 v2

getRanks :: Punkte -> [(Integer,Verein)]
getRanks p = getRanks' $ map fst $ sortBy sortRanks $ map (\verein -> (verein, toInt(p verein))) vereine
   where getRanks' [] = []
         getRanks' (x:xs) = (fromIntegral(length xs + 1) , x) : getRanks' xs 

getSpielerWithRank :: (Integer -> Bool) -> Saison -> [(SpielerId,Verein)]
getSpielerWithRank f saison@(punkte, Kd kader, _) = concat $ map (getPlayerTuples kader) $ getVereinWithRank f saison

getPlayerTuples :: (Verein -> [SpielerId]) -> Verein -> [(SpielerId, Verein)]
getPlayerTuples f verein = map (\spieler -> (spieler, verein)) $ f verein

getVereinWithRank :: (Integer -> Bool) -> Saison -> [Verein]
getVereinWithRank f (punkte, _, _) = map snd $ filter (\points -> f (fst points)) ranks
    where ranks = getRanks punkte

get_spm :: Historie -> [SpielerId]
get_spm h = sort $ best
  where bestPlayers = map fst $ nub $ concat $ map (getSpielerWithRank (1 ==)) h -- list of unique (SpielerId,Verein) tuples
        best = filterByMaxLength $ group $ sort $ bestPlayers -- list of (SpielerId, Count of different Verein's)

sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]
primes = sieve [2..] 

isPrime :: Integer -> Bool
isPrime x = isPrime' x primes
   where isPrime' n (x:xs) | x > n = False
                           | x == n = True
                           | otherwise = isPrime' n xs

filterByMaxLength :: [[a]] -> [a]
filterByMaxLength l = map head $ filter (\x -> length x == maxLength) l
    where maxLength = length $ maximumBy (comparing length) l

get_mdhm :: Historie -> [Verein]
get_mdhm h = sort $ filterByMaxLength $ group $ sort $ concat $ map (getVereinWithRank isPrime) h 


isPowerOfTwo :: Integer -> Bool
isPowerOfTwo 0 = False
isPowerOfTwo 1 = True
isPowerOfTwo n = ( n `mod` 2 == 0) && isPowerOfTwo (n `div` 2)

get_mdhi :: Historie -> [Verein]
get_mdhi h = sort $ nub $ filterByMaxLength $ group $ sort $ concat $ map (getVereinWithRank isPowerOfTwo) h 

get_pv :: Historie -> [SpielerId]
get_pv h = sort $ filterByMaxLength $ group $ sort badluck
  where winners = map fst $ concat $ map (getSpielerWithRank (1 ==)) h
        losers = map fst $ concat $ map (getSpielerWithRank (2 ==)) h
        badluck = [ x | x <- losers, not (elem x winners) ] 

get_ugr :: Historie -> [SpielerId]
get_ugr h = sort $ nub $ map fst $ filterByMaxLength $ sort $ concat $ map (getSpielerWithRank (lastPlace ==)) h
  where lastPlace = length vereine

get_tsp :: Historie -> [TrainerId]
get_tsp h = []

get_taz :: Historie -> [TrainerId]
get_taz h = []

get_vsz :: Historie -> [Verein]
get_vsz h = []

get_tmv :: Historie -> [TrainerId]
get_tmv h = []

get_tps :: Historie -> [TrainerId]
get_tps h = []