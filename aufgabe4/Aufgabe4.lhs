> module Aufgabe4 where 

> data Nat = Z | S Nat deriving Show

> plusN :: Nat -> Nat -> Nat 
> plusN n Z = n
> plusN Z m = m
> plusN n (S m) = plusN (S n) m 

> minusN :: Nat -> Nat -> Nat
> minusN Z m = Z
> minusN n Z = n
> minusN (S n) (S m) = minusN n m 

> timesN :: Nat -> Nat -> Nat
> timesN Z _ = Z
> timesN _ Z = Z 
> timesN n (S m) = plusN n $ timesN n (m)

> divN :: Nat -> Nat -> Nat
> divN Z _ = Z
> divN n m | leN n m = Z 
> divN n m = plusN (S Z) $ divN (minusN n m) m

> modN :: Nat -> Nat -> Nat
> modN Z _ = Z
> modN n m = minusN n $ timesN m $ divN n m 

> powerN :: Nat -> Nat -> Nat
> powerN _ Z = (S Z)
> powerN n (S Z) = n
> powerN n (S m) = timesN n $ powerN n m

> eqN :: Nat -> Nat -> Bool 
> eqN Z Z = True
> eqN Z _ = False
> eqN _ Z = False 
> eqN (S n) (S m) = eqN n m 

> neqN :: Nat -> Nat -> Bool
> neqN n m = not $ eqN n m

> grN :: Nat -> Nat -> Bool
> grN Z _ = False
> grN _ Z = True 
> grN (S n) (S m) = grN n m   

> leN :: Nat -> Nat -> Bool
> leN n m = neqN n m && not (grN n m)
 
> grEqN :: Nat -> Nat -> Bool
> grEqN n m = not $ leN n m 

> leEqN :: Nat -> Nat -> Bool
> leEqN n m = not $ grN n m 

> type PosRat = (Nat, Nat)

> -- isCanPR :: PosRat -> Bool
> --

> -- mkCanPR :: PosRat -> PosRat
> 
