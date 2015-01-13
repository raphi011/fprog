module Aufgabe8 where

import Data.List
import Control.Applicative

toInt :: Nat -> Integer
toInt Z = 0
toInt (S n) = 1 + toInt n

data Verein = Sturm | WAC | Austria | WrNeustadt | RBSbg | Groedig | Rapid | Admira | Ried | Altach deriving (Eq,Ord,Show)

type Spiel = (Verein,Verein)
newtype Spieltag = St (Spiel,Spiel,Spiel,Spiel,Spiel) deriving (Eq,Show)

data Restprogramm = Rp Spieltag Spieltag Spieltag

type Punkte = (Verein -> Nat)
type Budget = (Verein -> Nat)

data Nat = Z | S Nat deriving (Eq,Ord,Show)

data Herbstmeister = NochOffen | AlsHMstehtfest Verein deriving (Eq,Show)

vereine = [Sturm, WAC, Austria, WrNeustadt, RBSbg, Groedig, Rapid, Admira, Ried, Altach]

{- isValidBudget
 - returns true if budgets are distinct
 -}
isValidBudget :: Budget -> Bool
isValidBudget b = length (nub (map b vereine )) == length vereine

{- isValidSpiel
 - returns true if (x,y) x != y
 -}
isValidSpiel :: Spiel -> Bool
isValidSpiel (t1,t2) = t1 /= t2

{- isValidSpieltag
 - returns true if all games are valid and all teams have played
 -}
isValidSpieltag :: Spieltag -> Bool
isValidSpieltag st = all isValidSpiel games && (length $ nub teams) == (length vereine)
    where games = gamesOfDay st
          teams = [fst,snd] <*> games

{- gamesOfDay
 - returns all games of a gameday
 -}
gamesOfDay :: Spieltag -> [Spiel]
gamesOfDay (St (s1, s2, s3, s4, s5)) = [s1,s2,s3,s4,s5] 

{- isSet
 - returns true if a list is a set ( no duplicate values )
 -}
isSet :: Eq a => [a] -> Bool
isSet a = (length $ nub a) == (length a)

{- isValidRestprogramm
 - returns true if all gamedays are valid and there are no duplicate games
 -}
isValidRestprogramm :: Restprogramm -> Bool
isValidRestprogramm (Rp s1 s2 s3) = isValidSpieltag s1 && isValidSpieltag s2 && isValidSpieltag s3 && isSet games
    where games = concat $ map gamesOfDay [s1,s2,s3]


{- comparePoints
 - compares the points and if they are equal
 - the team with the lower budget is greater
 -}
comparePoints :: Punkte -> Budget -> Verein -> Verein -> Ordering
comparePoints p b v1 v2 | p1 == p2 = compare (b v1) (b v2)
                        | otherwise = compare p2 p1
    where p1 = p v1
          p2 = p v2                     

{- mkTabelle 
 - returns a list where the teams are ordered
 - by their rank (ascending) 
 -}
mkTabelle :: Punkte -> Budget -> [Verein]
mkTabelle punkte budget | not $ isValidBudget budget = error "Ungueltige Eingabe"
                        | otherwise = sortBy (\x y -> comparePoints punkte budget x y) $ vereine

{- hm_fix 
 - returns a team if it's first place
 - or 'NochOffen' if it can't be determined yet
 -}
hm_fix :: Punkte -> Budget -> Restprogramm -> Herbstmeister
hm_fix p b r | ((toInt $ p second) + 9) < (toInt $ p first) = AlsHMstehtfest first
	     | otherwise = NochOffen 
	where tabelle = mkTabelle p b
	      first = tabelle !! 0
	      second = tabelle !! 1

{- hm_ausEigenerKraft
 - 
 -}
hm_ausEigenerKraft :: Punkte -> Budget -> Restprogramm -> [Verein]
hm_ausEigenerKraft p b r = filter (\x -> toInt (p x) + 9 >= pointsFirst) $ tail tabelle  
	where tabelle = mkTabelle p b
	      pointsFirst = toInt $ p $ head tabelle  


{- hm_alleMitRechnerischerChance 
 - 
 -}
hm_alleMitRechnerischerChance :: Punkte -> Budget -> Restprogramm -> [Verein]
hm_alleMitRechnerischerChance p b r = []

{- vhm_alleMitRechnerischerChance 
 -
 -}
vhm_alleMitRechnerischerChance :: Punkte -> Budget -> Restprogramm -> [Verein]
vhm_alleMitRechnerischerChance p b r =  []
















