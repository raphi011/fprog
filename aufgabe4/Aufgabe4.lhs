> module Aufgabe4 where 

> data Nat = Z | S Nat deriving Show

Add two Nats 

> plusN :: Nat -> Nat -> Nat 
> plusN n Z = n
> plusN Z m = m
> plusN n (S m) = plusN (S n) m 

Subtract two Nats, result can't
be lower than Z

> minusN :: Nat -> Nat -> Nat
> minusN Z m = Z
> minusN n Z = n
> minusN (S n) (S m) = minusN n m 

Multiply two Nats

> timesN :: Nat -> Nat -> Nat
> timesN Z _ = Z
> timesN _ Z = Z 
> timesN n (S m) = plusN n $ timesN n (m)

'Integer' division of two Nats

> divN :: Nat -> Nat -> Nat
> divN Z _ = Z
> divN n m | leN n m = Z 
> divN n m = plusN (S Z) $ divN (minusN n m) m

Modulo operation on two Nats

> modN :: Nat -> Nat -> Nat
> modN Z _ = Z
> modN n m = minusN n $ timesN m $ divN n m 

Calculates first Nat param to the 
power of the second

> powerN :: Nat -> Nat -> Nat
> powerN _ Z = (S Z)
> powerN n (S Z) = n
> powerN n (S m) = timesN n $ powerN n m

Equals operator on two Nats

> eqN :: Nat -> Nat -> Bool 
> eqN Z Z = True
> eqN Z _ = False
> eqN _ Z = False 
> eqN (S n) (S m) = eqN n m

Not equals operator on two nats 

> neqN :: Nat -> Nat -> Bool
> neqN n m = not $ eqN n m

Greater operator, True if first
Nat is greater than the second

> grN :: Nat -> Nat -> Bool
> grN Z _ = False
> grN _ Z = True 
> grN (S n) (S m) = grN n m   

Lesser operator, True if first 
Nat is lesser than the second

> leN :: Nat -> Nat -> Bool
> leN n m = neqN n m && not (grN n m)
 
Greater or equals, True if the first
Nat is equals or greater than the second

> grEqN :: Nat -> Nat -> Bool
> grEqN n m = not $ leN n m 

Lesser or equals, True if the first 
Nat is equals or lesser than the second

> leEqN :: Nat -> Nat -> Bool
> leEqN n m = not $ grN n m 

> type PosRat = (Nat, Nat)

Tests PosRat if it's in
canonical form

> isCanPR :: PosRat -> Bool
> isCanPR (Z, S Z) = True
> isCanPR (_, Z) = False 
> isCanPR (n,m) = eqN (ggtPR (n,m)) (S Z)

Calculates ggt of nominator and denominator

> ggtPR :: PosRat -> Nat 
> ggtPR (n,Z) = n
> ggtPR (n,m) = ggtPR (m,modN n m)

Returns PosRat in canonical form
 
> mkCanPR :: PosRat -> PosRat
> mkCanPR (_,Z) = (Z,Z)
> mkCanPR r | isCanPR r = r
> mkCanPR (n,m) = (divN n ggt, divN m ggt)
> 	where ggt = ggtPR (n,m) 

Adds two PosRats

> plusPR :: PosRat -> PosRat -> PosRat
> plusPR (n1, m1) (n2, m2) = mkCanPR (plusN (timesN n1 m2) (timesN n2 m1), timesN m1 m2)

Subtracts two PosRats

> minusPR :: PosRat -> PosRat -> PosRat
> minusPR (n1, m1) (n2, m2) = mkCanPR (minusN (timesN n1 m2) (timesN n2 m1), timesN m1 m2)

Multiplys two PosRats

> timesPR :: PosRat -> PosRat -> PosRat
> timesPR (n1, m1) (n2, m2) = mkCanPR (timesN n1 n2, timesN m1 m2)

Divides two PosRats

> divPR :: PosRat -> PosRat -> PosRat
> divPR	(n1, m1) (n2, m2) = mkCanPR (timesN n1 m2, timesN n2 m1)

Equality operation on two PosRats

> eqPR :: PosRat -> PosRat -> Bool
> eqPR (n1, m1) (n2, m2) = neqN m1 Z && neqN m2 Z && eqN (timesN n1 m2) (timesN n2 m1)

Not Equals operation on two PosRats

> neqPR :: PosRat -> PosRat -> Bool
> neqPR (n1, m1) (n2, m2) = neqN m1 Z && neqN m2 Z && not (eqN (timesN n1 m2) (timesN n2 m1))

Greater operation on two PosRats

> grPR :: PosRat -> PosRat -> Bool
> grPR (n1, n2) (n3, n4) = grN (timesN n1 n4) (timesN n2 n3)

Lesser operation on two PosRats

> lePR :: PosRat -> PosRat -> Bool
> lePR (n1, n2) (n3, n4) = leN (timesN n1 n4) (timesN n2 n3)

Greater or Equals operation on two PosRats

> grEqPR :: PosRat -> PosRat -> Bool
> grEqPR b1 b2 = eqPR b1 b2 || (grPR b1 b2)

Lesser or Equals operation on two PosRats

> leEqPR :: PosRat -> PosRat -> Bool
> leEqPR b1 b2 = eqPR b1 b2 || (lePR b1 b2)

