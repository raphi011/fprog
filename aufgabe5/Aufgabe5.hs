module Aufgabe5 where

data Tree = Node (Label -> Label) Label [Tree]
type Label = Int

{- tcm
 - tests every label for first bool function,
 - if true it applies the given transformation
 - else it applies the transformation in the node
 -}
tcm :: (Label -> Bool) -> (Label -> Label) -> Tree -> Tree
tcm cond func (Node f l t)	| cond l = Node f (func l) $ map (tcm cond func) t
				| otherwise = Node f (f l) $ map (tcm cond func) t 

{- tzp
 - zips two trees, if the labels are equal the label 
 - is altered with the composition of f1 and f2, else 
 - the given function is used 
 -}
tzp :: (Label -> Label -> Label) -> Tree -> Tree -> Tree
tzp transf (Node f1 l1 t1) (Node f2 l2 t2)	| l1 == l2 = Node (transf l1) ((f1 . f2) l1) $ zipWith (tzp transf) t1 t2 
						| otherwise = Node (f1 . f2) (transf l1 l2) $ zipWith (tzp transf) t1 t2

{- tmax 
 - returns the function the returns the biggest value
 - when l is applied to it 
 -}
tmax :: Label -> Tree -> (Label -> Label)
tmax l (Node f _ []) = f  
tmax l (Node f _ t)  = foldl (\acc tree -> tmax' (tmax l tree) acc) f t 
	where tmax' f1 f2 | (f1 l) > (f2 l) = f1
			  | otherwise = f2 

data STree = SNode Label [STree] deriving (Eq, Show)


{- t2st
 - create a simple tree from a tree
 -}
t2st :: Tree -> STree
t2st (Node _ l t) = SNode l $ map t2st t

{- tsum
 - sums up all labels of a stree
 -}
tsum :: STree -> Label
tsum (SNode l t) = foldl (\acc stree -> tsum stree + acc  ) l t

{- tdepth
 - returns the depth of a stree
 -} 
tdepth :: STree -> Label
tdepth (SNode _ t)  = foldl (\acc stree -> max ((tdepth stree) + 1) acc) 1 t 
