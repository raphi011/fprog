module Aufgabe6 where

import Data.List

plusN :: Nat -> Nat -> Nat 
plusN n Z = n
plusN Z m = m
plusN n (S m) = plusN (S n) m 

minusN :: Nat -> Nat -> Nat
minusN Z m = Z
minusN n Z = n
minusN (S n) (S m) = minusN n m 

timesN :: Nat -> Nat -> Nat
timesN Z _ = Z
timesN _ Z = Z 
timesN n (S m) = plusN n $ timesN n (m)

divN :: Nat -> Nat -> Nat
divN Z _ = Z
divN n m | leN n m = Z 
divN n m = plusN (S Z) $ divN (minusN n m) m

modN :: Nat -> Nat -> Nat
modN Z _ = Z
modN n m = minusN n $ timesN m $ divN n m 

powerN :: Nat -> Nat -> Nat
powerN _ Z = (S Z)
powerN n (S Z) = n
powerN n (S m) = timesN n $ powerN n m

eqN :: Nat -> Nat -> Bool 
eqN Z Z = True
eqN Z _ = False
eqN _ Z = False 
eqN (S n) (S m) = eqN n m

neqN :: Nat -> Nat -> Bool
neqN n m = not $ eqN n m

grN :: Nat -> Nat -> Bool
grN Z _ = False
grN _ Z = True 
grN (S n) (S m) = grN n m   

leN :: Nat -> Nat -> Bool
leN n m = neqN n m && not (grN n m)
 
grEqN :: Nat -> Nat -> Bool
grEqN n m = not $ leN n m 

leEqN :: Nat -> Nat -> Bool
leEqN n m = not $ grN n m 

isCanPR :: PosRat -> Bool
isCanPR (Z, S Z) = True
isCanPR (_, Z) = False 
isCanPR (n,m) = eqN (ggtPR (n,m)) (S Z)

ggtPR :: PosRat -> Nat 
ggtPR (n,Z) = n
ggtPR (n,m) = ggtPR (m,modN n m)

mkCanPR :: PosRat -> PosRat
mkCanPR (_,Z) = (Z,Z)
mkCanPR r | isCanPR r = r
mkCanPR (n,m) = (divN n ggt, divN m ggt)
	where ggt = ggtPR (n,m) 

plusPR :: PosRat -> PosRat -> PosRat
plusPR (n1, m1) (n2, m2) = mkCanPR (plusN (timesN n1 m2) (timesN n2 m1), timesN m1 m2)

minusPR :: PosRat -> PosRat -> PosRat
minusPR (n1, m1) (n2, m2) = mkCanPR (minusN (timesN n1 m2) (timesN n2 m1), timesN m1 m2)

timesPR :: PosRat -> PosRat -> PosRat
timesPR (n1, m1) (n2, m2) = mkCanPR (timesN n1 n2, timesN m1 m2)

divPR :: PosRat -> PosRat -> PosRat
divPR	(n1, m1) (n2, m2) = mkCanPR (timesN n1 m2, timesN n2 m1)

eqPR :: PosRat -> PosRat -> Bool
eqPR (n1, m1) (n2, m2) = neqN m1 Z && neqN m2 Z && eqN (timesN n1 m2) (timesN n2 m1)

neqPR :: PosRat -> PosRat -> Bool
neqPR (n1, m1) (n2, m2) = neqN m1 Z && neqN m2 Z && not (eqN (timesN n1 m2) (timesN n2 m1))

grPR :: PosRat -> PosRat -> Bool
grPR (n1, n2) (n3, n4) = grN (timesN n1 n4) (timesN n2 n3)

lePR :: PosRat -> PosRat -> Bool
lePR (n1, n2) (n3, n4) = leN (timesN n1 n4) (timesN n2 n3)

grEqPR :: PosRat -> PosRat -> Bool
grEqPR b1 b2 = eqPR b1 b2 || (grPR b1 b2)

leEqPR :: PosRat -> PosRat -> Bool
leEqPR b1 b2 = eqPR b1 b2 || (lePR b1 b2)

toNat :: Integer -> Nat
toNat n
    | n == 0 = Z
    | otherwise = S (toNat (n-1))

data Nat			= Z | S Nat
type PosRat 		= (Nat, Nat)
type Skalar			= PosRat
type ProtoMatrix	= [[Skalar]]
newtype Matrix		= M [[Skalar]]
type Fuellwert		= Integer

mkMatrix :: ProtoMatrix -> Fuellwert -> Matrix
mkMatrix pm i = M $ map (mkRow i (maximum (map length pm))) pm 

mkRow :: Fuellwert -> Int -> [Skalar] -> [Skalar]
mkRow i len row = take (max 1 len) (row ++ repeat (toNat (abs i), toNat 1))

data OktoZiffern = E | Zw | D | V | F | Se | Si | N deriving (Eq, Show)
type OktoZahlen = [OktoZiffern]
