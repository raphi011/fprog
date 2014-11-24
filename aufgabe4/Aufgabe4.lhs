> module Aufgabe4 where 

> data Nat = Z | S Nat deriving Show

> plusN :: Nat -> Nat -> Nat 
> plusN n = S (n)

> minusN :: Nat -> Nat -> Nat
> minusN Z = Z
> minusN (S m) = m


