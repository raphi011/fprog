module Aufgabe7 where

import Data.List
import Data.Ord 

{- toInt
 - converts Nat to Int
 -}
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

{- sortRanks
 - sorting function for 'Verein' rankings,
 - used by sortBy 
 -}
sortRanks :: (Verein, Integer) -> (Verein, Integer) -> Ordering
sortRanks (v1, p1) (v2, p2) | p1 > p2 = GT
                            | p1 < p2 = EQ
                            | otherwise = compare v2 v1

{- getRanks 
 - calculates the ranks of every Verein
 - for a season
 -}
getRanks :: Punkte -> [(Integer,Verein)]
getRanks p = getRanks' $ map fst $ sortBy sortRanks $ map (\verein -> (verein, toInt(p verein))) vereine
   where getRanks' [] = []
         getRanks' (x:xs) = (fromIntegral(length xs + 1) , x) : getRanks' xs 

{- getSpielerWithRank
 - returns a list of players from a season with a given rank
 -}
getSpielerWithRank :: (Integer -> Bool) -> Saison -> [(SpielerId,Verein)]
getSpielerWithRank f saison@(punkte, Kd kader, _) = concat $ map (getPlayerTuples kader) $ getVereinWithRank f saison

{- getPlayerTuples
 - list of players of a team
 -}
getPlayerTuples :: (Verein -> [SpielerId]) -> Verein -> [(SpielerId, Verein)]
getPlayerTuples f verein = map (\spieler -> (spieler, verein)) $ f verein

{- getVereinWithRank
 - list of teams with a certain rank
 -}
getVereinWithRank :: (Integer -> Bool) -> Saison -> [Verein]
getVereinWithRank f (punkte, _, _) = map snd $ filter (\points -> f (fst points)) ranks
    where ranks = getRanks punkte

{- get_spm
 - returns all players that have the most championships
 - with different teams
 -}
get_spm :: Historie -> [SpielerId]
get_spm h = sort $ best
  where bestPlayers = map fst $ nub $ concat $ map (getSpielerWithRank (1 ==)) h -- list of unique (SpielerId,Verein) tuples
        best = filterByMaxLength $ group $ sort $ bestPlayers -- list of (SpielerId, Count of different Verein's)

sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]
primes = sieve [2..] 

{- isPrime
 - returns True if value is a prime number
 -}
isPrime :: Integer -> Bool
isPrime x = isPrime' x primes
   where isPrime' n (x:xs) | x > n = False
                           | x == n = True
                           | otherwise = isPrime' n xs

{- filterByMaxLength
 - compares the length of each grouped sublist and returns
 - the head element of the sublists that have the most
 - elements ( maximum length )  
 -}
filterByMaxLength :: [[a]] -> [a]
filterByMaxLength l = map head $ filter (\x -> length x == maxLength) l
    where maxLength = length $ maximumBy (comparing length) l

{- get_mdhm
 - gets all teams that have the most 
 - prime number rankings
 -}
get_mdhm :: Historie -> [Verein]
get_mdhm h = sort $ filterByMaxLength $ group $ sort $ concat $ map (getVereinWithRank isPrime) h 

{- isPowerOfTwo
 - returns True if the value is a power of two 
 -}
isPowerOfTwo :: Integer -> Bool
isPowerOfTwo 0 = False
isPowerOfTwo 1 = True
isPowerOfTwo n = ( n `mod` 2 == 0) && isPowerOfTwo (n `div` 2)

{- get_mdhi
 - gets all teams that finished most often
 - on a rank of the power of two
 -}
get_mdhi :: Historie -> [Verein]
get_mdhi h = sort $ nub $ filterByMaxLength $ group $ sort $ concat $ map (getVereinWithRank isPowerOfTwo) h 

{- get_pv 
 - gets all players that were on second place the most
 - without ever winning
 -}
get_pv :: Historie -> [SpielerId]
get_pv h =  reverse . sort $ filterByMaxLength $ group $ sort badluck
  where winners = nub $ map fst $ concat $ map (getSpielerWithRank (1 ==)) h
        losers = map fst $ concat $ map (getSpielerWithRank (2 ==)) h
        badluck = [ x | x <- losers, not (x `elem` winners) ] 

{- get_ugr
 - gets the players which were last
 - the most 
 -}
get_ugr :: Historie -> [SpielerId]
get_ugr h = reverse . sort $ filterByMaxLength $ group $ sort $ map fst $ concat $ map (getSpielerWithRank (lastPlace ==)) h
  where lastPlace = toInteger $ length vereine

{- getTrainerWithRank
 - gets a list of (TrainerId,Verein) tuples
 - of all the trainers with a certain rank
 -}
getTrainerWithRank :: (Integer -> Bool) -> Saison -> [(TrainerId,Verein)]
getTrainerWithRank f saison@(_, _, Tr trainer) = map (\x -> (trainer x, x)) $ getVereinWithRank f saison

{- get_tsp
 - gets every trainer that finished with the rank
 - 1-3 the most
 -}
get_tsp :: Historie -> [TrainerId]
get_tsp h = sort $ filterByMaxLength $ group $ sort $ map fst $ concat $ map (getTrainerWithRank (`elem` [1,2,3])) h

{- get_taz
 - gets every trainer that finished with the 
 - last three ranks the most
 -}
get_taz :: Historie -> [TrainerId]
get_taz h = sort $ filterByMaxLength $ group $ sort $ map fst $ nub $ concat $ map (getTrainerWithRank (`elem` lastThree)) h
  where anzahlVereine = length vereine 
        lastThree = [ fromIntegral x | x <- [anzahlVereine - 2 .. anzahlVereine]]

{- getAllTrainers
 - gets every trainer of the given season
 -}
getAllTrainers :: Saison -> [(TrainerId,Verein)]
getAllTrainers (_,_, Tr trainer) = map (\x -> (trainer x, x)) vereine

{- get_vsz
 - returns the teams that relied on the most trainers
 -}
get_vsz :: Historie -> [Verein]
get_vsz h = sort $ map snd $ filterByMaxLength $ groupBy (\x y -> snd x == snd y) $ sortBy (\x y -> compare (snd x) (snd y)) $ nub $ concat $ map getAllTrainers h

{- get_tmv
 - gets all trainers that coached the most teams
 -}
get_tmv :: Historie -> [TrainerId]
get_tmv h = sort $ map fst $ filterByMaxLength $ groupBy (\x y -> fst x == fst y) $ sort $ nub $ concat $ map getAllTrainers h

{- get_tps
 - gets all trainers that had the longest 
 - running careers with one team
 -}
get_tps :: Historie -> [TrainerId]
get_tps h = map fst $ filter (\(_,y) -> y == maxDuration) history3
  where history = groupBy (\x y -> fst x == fst y) $ sortBy (\x y -> compare (fst x) (fst y)) $ concat $ map getAllTrainers h
        history2 = map (foldl (\(x,y) cur -> (x, (snd cur) : y)) (TId Z,[])) history
        history3 = map (\(x,y) -> (x,length $ filterByMaxLength $ group $ y)) history2
        maxDuration = snd $ maximumBy (comparing snd) history3