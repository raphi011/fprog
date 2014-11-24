-- Funktionale Programmierung WS2014
-- Exercise: 4
-- File: HUnit4.hs
-- HUnit test cases

import Aufgabe4
import Test.HUnit

main = runTestTT $ TestList [test1,test2]

test1 = TestLabel "Test: Teil1" $ (
        TestList [
        TestCase $ assertBool "plusN (S (S Z)) (S (S (S Z)))" (equalNat (S (S (S (S (S Z))))) (plusN (S (S Z)) (S (S (S Z))))),
        TestCase $ assertBool "plusN (Z) (Z)" (equalNat (Z) (plusN (Z) (Z))),

        TestCase $ assertBool "minusN (S (S Z)) (S (S (S Z)))" (equalNat (Z) (minusN (S (S Z)) (S (S (S Z))))),
        TestCase $ assertBool "minusN (Z) (Z)" (equalNat (Z) (minusN (Z) (Z))),
        TestCase $ assertBool "minusN (Z) (S Z)" (equalNat (Z) (minusN (Z) (S Z))),
        TestCase $ assertBool "minusN (S Z) (Z)" (equalNat (S Z) (minusN (S Z) (Z))),

        TestCase $ assertBool "timesN (S (S Z)) (S (S (S Z)))" (equalNat (S (S (S (S (S (S Z)))))) (timesN (S (S Z)) (S (S (S Z))))),
        TestCase $ assertBool "timesN (Z) (Z)" (equalNat (Z) (timesN (Z) (Z))),
        TestCase $ assertBool "timesN (Z) (S Z)" (equalNat (Z) (timesN (Z) (S Z))),
        TestCase $ assertBool "timesN (S Z) (Z)" (equalNat (Z) (timesN (S Z) (Z))),
        TestCase $ assertBool "timesN (S Z) (S Z)" (equalNat (S Z) (timesN (S Z) (S Z))),

        TestCase $ assertBool "divN (S (S Z)) (S (S (S Z)))" (equalNat (Z) (divN (S (S Z)) (S (S (S Z))))),
        TestCase $ assertBool "divN (Z) (S Z)" (equalNat (Z) (divN (Z) (S Z))),
        TestCase $ assertBool "divN (S Z) (S Z)" (equalNat (S Z) (divN (S Z) (S Z))),
        
        TestCase $ assertBool "modN (S (S Z)) (S (S (S Z)))" (equalNat (S (S Z)) (modN (S (S Z)) (S (S (S Z))))),
        TestCase $ assertBool "modN (Z) (S Z)" (equalNat (Z) (modN (Z) (S Z))),
        TestCase $ assertBool "modN (S (S (S Z))) (S Z)" (equalNat (Z) (modN (S (S (S Z))) (S Z))),
        TestCase $ assertBool "modN (S (S (S Z))) (S (S Z))" (equalNat (S Z) (modN (S (S (S Z))) (S (S Z)))),

        TestCase $ assertBool "powerN (S (S Z)) (S (S (S Z)))" (equalNat (S (S (S (S (S (S (S (S Z)))))))) (powerN (S (S Z)) (S (S (S Z))))),
        TestCase $ assertBool "powerN (S Z) (Z)" (equalNat (S Z) (powerN (S Z) (Z))),
        TestCase $ assertBool "powerN (S (S Z)) (Z)" (equalNat (S Z) (powerN (S (S Z)) (Z))),

        TestCase $ assertEqual "eqN (S (S Z)) (S (S (S Z)))" (False) (eqN (S (S Z)) (S (S (S Z)))),
        TestCase $ assertEqual "eqN (S Z) (S Z)" (True) (eqN (S Z) (S Z)),
        TestCase $ assertEqual "eqN (Z) (Z)" (True) (eqN (Z) (Z)),

        TestCase $ assertEqual "neqN (S (S Z)) (S (S (S Z)))" (True) (neqN (S (S Z)) (S (S (S Z)))),
        TestCase $ assertEqual "neqN (S Z) (S Z)" (False) (neqN (S Z) (S Z)),
        TestCase $ assertEqual "neqN (Z) (Z)" (False) (neqN (Z) (Z)),

        TestCase $ assertEqual "grN (S (S Z)) (S (S (S Z)))" (False) (grN (S (S Z)) (S (S (S Z)))),
        TestCase $ assertEqual "grN (S Z) (Z)" (True) (grN (S Z) (Z)),
        TestCase $ assertEqual "grN (Z) (S Z)" (False) (grN (Z) (S Z)),

        TestCase $ assertEqual "leN (S (S Z)) (S (S (S Z)))" (True) (leN (S (S Z)) (S (S (S Z)))),
        TestCase $ assertEqual "leN (S Z) (Z)" (False) (leN (S Z) (Z)),
        TestCase $ assertEqual "leN (Z) (S Z)" (True) (leN (Z) (S Z)),

        TestCase $ assertEqual "grEqN (S (S Z)) (S (S (S Z)))" (False) (grEqN (S (S Z)) (S (S (S Z)))),
        TestCase $ assertEqual "grEqN (S (S (S (S Z)))) (S (S (S Z)))" (True) (grEqN (S (S (S (S Z)))) (S (S (S Z)))),
        TestCase $ assertEqual "grEqN (S Z) (Z)" (True) (grEqN (S Z) (Z)),
        TestCase $ assertEqual "grEqN (Z) (S Z)" (False) (grEqN (Z) (S Z)),
        TestCase $ assertEqual "grEqN (Z) (Z)" (True) (grEqN (Z) (Z)),
        TestCase $ assertEqual "grEqN (S Z) (S Z)" (True) (grEqN (S Z) (S Z)),

        TestCase $ assertEqual "leEqN (S (S Z)) (S (S (S Z)))" (True) (leEqN (S (S Z)) (S (S (S Z)))),
        TestCase $ assertEqual "leEqN (S (S (S (S Z)))) (S (S (S Z)))" (False) (leEqN (S (S (S (S Z)))) (S (S (S Z)))),
        TestCase $ assertEqual "leEqN (S Z) (Z)" (False) (leEqN (S Z) (Z)),
        TestCase $ assertEqual "leEqN (Z) (S Z)" (True) (leEqN (Z) (S Z)),
        TestCase $ assertEqual "leEqN (Z) (Z)" (True) (leEqN (Z) (Z)),
        TestCase $ assertEqual "leEqN (S Z) (S Z)" (True) (leEqN (S Z) (S Z))
        ]
    )

test2 = TestLabel "Test: Teil2" $ (
        TestList [
        TestCase $ assertEqual "isCanPR (S (S Z),S (S Z))" (False) (isCanPR (S (S Z),S (S Z))),
        TestCase $ assertEqual "isCanPR (S (S Z),S Z)" (True) (isCanPR (S (S Z),S Z)),
        TestCase $ assertEqual "isCanPR ((S Z), S (S Z))" (True) (isCanPR ((S Z), S (S Z))),
        TestCase $ assertEqual "isCanPR ((Z), (Z))" (False) (isCanPR ((Z), (Z))),
        TestCase $ assertEqual "isCanPR ((S Z), (Z))" (False) (isCanPR ((S Z), (Z))),
        TestCase $ assertEqual "isCanPR ((Z), (S Z))" (True) (isCanPR ((Z), (S Z)))

        --TestCase $ assertBool "mkCanPR (S (S Z),S (S Z))" (equalPR (S Z,S Z) (mkCanPR (S (S Z),S (S Z)))),
        --TestCase $ assertBool "mkCanPR (S (S (S Z)),S (S Z))" (equalPR (S (S (S Z)),S (S Z)) (mkCanPR (S (S (S Z)),S (S Z)))),
        --TestCase $ assertBool "mkCanPR ((S Z),(S Z))" (equalPR (S Z,S Z) (mkCanPR ((S Z),(S Z)))),
        --TestCase $ assertBool "mkCanPR (S (S Z),(S Z))" (equalPR (S (S Z),S Z) (mkCanPR (S (S Z),(S Z)))),
        --TestCase $ assertBool "mkCanPR (S (S Z),(Z))" (equalPR (Z,Z) (mkCanPR (S (S Z),(Z)))),

        --TestCase $ assertBool "plusPR (S Z,S (S (S Z))) (S Z,S Z)" (equalPR (S (S (S (S Z))),(S (S (S Z)))) (plusPR (S Z,S (S (S Z))) (S Z,S Z))),
        --TestCase $ assertBool "plusPR (S (S Z), S (S Z)) (S (S (S Z)), S (S (S Z)))" (equalPR (S (S Z), S Z) (plusPR (S (S Z), S (S Z)) (S (S (S Z)), S (S (S Z))))),
        --TestCase $ assertBool "plusPR (S Z, S (S Z)) (S Z, S (S Z))" (equalPR (S Z ,S Z) (plusPR (S Z, S (S Z)) (S Z, S (S Z)))),
        --TestCase $ assertBool "plusPR (Z, S (S Z)) (S Z, S (S Z))" (equalPR (S Z, S (S Z)) (plusPR (Z, S (S Z)) (S Z, S (S Z)))),

        --TestCase $ assertBool "minusPR (S Z,S Z) (S Z,S (S (S Z)))" (equalPR (S (S Z),(S (S (S Z)))) (minusPR (S Z,S Z) (S Z,S (S (S Z))))),
        --TestCase $ assertBool "minusPR (S (S Z), S (S Z)) (S (S (S Z)), S (S (S Z)))" (equalPR (Z, S Z) (minusPR (S (S Z), S (S Z)) (S (S (S Z)), S (S (S Z))))),
        --TestCase $ assertBool "minusPR (S Z, S (S Z)) (Z, S (S Z))" (equalPR (S Z, S (S Z)) (minusPR (S Z, S (S Z)) (Z, S (S Z)))),

        --TestCase $ assertBool "timesPR (S Z,S (S Z)) (S Z,S (S Z))" (equalPR (S Z,S (S (S (S Z)))) (timesPR (S Z,S (S Z)) (S Z,S (S Z)))),
        --TestCase $ assertBool "timesPR (Z, S (S Z)) (S Z, S (S Z))" (equalPR (Z, S Z) (timesPR (Z, S (S Z)) (S Z, S (S Z)))),

        --TestCase $ assertBool "divPR (S Z,S (S Z)) (S Z,S (S Z))" (equalPR (S Z,S Z) (divPR (S Z,S (S Z)) (S Z,S (S Z)))),
        --TestCase $ assertBool "divPR (Z,S (S Z)) (S Z,S (S Z))" (equalPR (Z,S Z) (divPR (Z,S (S Z)) (S Z,S (S Z)))),
        --TestCase $ assertBool "divPR (S Z,S (S Z)) (Z,S (S Z))" (equalPR (Z,Z) (divPR (S Z,S (S Z)) (Z,S (S Z)))),

        --TestCase $ assertEqual "eqPR (S (S Z),S (S Z)) (S Z,S Z)" (True) (eqPR (S (S Z),S (S Z)) (S Z,S Z)),
        --TestCase $ assertEqual "eqPR (Z, S (S (S Z))) (Z, S Z)" (True) (eqPR (Z, S (S (S Z))) (Z, S Z)),
        --TestCase $ assertEqual "eqPR (S (S Z),Z) (S Z,S (S Z))" (False) (eqPR (S (S Z),Z) (S Z,S (S Z))),
        --TestCase $ assertEqual "eqPR (S (S Z), S Z) (S Z, S (S Z))" (False) (eqPR (S (S Z),S Z) (S Z,S (S Z))),

        --TestCase $ assertEqual "neqPR (S (S Z),Z) (S Z,S (S Z))" (False) (neqPR (S (S Z),Z) (S Z,S (S Z))),
        --TestCase $ assertEqual "neqPR (Z, S (S (S Z))) (Z, S Z)" (False) (neqPR (Z, S (S (S Z))) (Z, S Z)),
        --TestCase $ assertEqual "neqPR (S (S Z), S Z) (S Z, S (S Z))" (True) (neqPR (S (S Z),S Z) (S Z,S (S Z))),
        --TestCase $ assertEqual "neqPR (S (S Z),S (S Z)) (S Z,S Z)" (False) (neqPR (S (S Z),S (S Z)) (S Z,S Z)),

        --TestCase $ assertEqual "grPR (S Z,S Z) (S Z,S (S (S Z)))" (True) (grPR (S Z,S Z) (S Z,S (S (S Z)))),
        --TestCase $ assertEqual "grPR (S Z,S(S Z)) (S Z,S (S (S Z)))" (True) (grPR (S Z,S(S Z)) (S Z,S (S (S Z)))),
        --TestCase $ assertEqual "grPR (Z,S Z) (S Z,S (S (S Z)))" (False) (grPR (Z,S Z) (S Z,S (S (S Z)))),
        --TestCase $ assertEqual "grPR (S Z,S (S (S Z))) (S Z,S Z)" (False) (grPR (S Z,S (S (S Z))) (S Z,S Z)),

        --TestCase $ assertEqual "lePR (S Z,S (S Z)) (S (S Z),S (S (S (S Z))))" (False) (lePR (S Z,S (S Z)) (S (S Z),S (S (S (S Z))))),
        --TestCase $ assertEqual "lePR (S Z,S Z) (S Z,S (S (S Z)))" (False) (lePR (S Z,S Z) (S Z,S (S (S Z)))),
        --TestCase $ assertEqual "lePR (S Z,S(S Z)) (S Z,S (S (S Z)))" (False) (lePR (S Z,S(S Z)) (S Z,S (S (S Z)))),
        --TestCase $ assertEqual "lePR (Z,S Z) (S Z,S (S (S Z)))" (True) (lePR (Z,S Z) (S Z,S (S (S Z)))),
        --TestCase $ assertEqual "lePR (S Z,S (S (S Z))) (S Z,S Z)" (True) (lePR (S Z,S (S (S Z))) (S Z,S Z)),

        --TestCase $ assertEqual "grEqPR (S Z,S (S Z)) (S (S Z),S (S (S (S Z))))" (True) (grEqPR (S Z,S (S Z)) (S (S Z),S (S (S (S Z))))),
        --TestCase $ assertEqual "grEqPR (S Z,S Z) (S Z,S (S (S Z)))" (True) (grEqPR (S Z,S Z) (S Z,S (S (S Z)))),
        --TestCase $ assertEqual "grEqPR (S Z,S(S Z)) (S Z,S (S (S Z)))" (True) (grEqPR (S Z,S(S Z)) (S Z,S (S (S Z)))),
        --TestCase $ assertEqual "grEqPR (Z,S Z) (S Z,S (S (S Z)))" (False) (grEqPR (Z,S Z) (S Z,S (S (S Z)))),
        --TestCase $ assertEqual "grEqPR (S Z,S (S (S Z))) (S Z,S Z)" (False) (grEqPR (S Z,S (S (S Z))) (S Z,S Z)),

        --TestCase $ assertEqual "leEqPR (S (S Z),S (S Z)) (S Z,S Z)" (True) (leEqPR (S (S Z),S (S Z)) (S Z,S Z)),
        --TestCase $ assertEqual "leEqPR (S Z,S Z) (S Z,S (S (S Z)))" (False) (leEqPR (S Z,S Z) (S Z,S (S (S Z)))),
        --TestCase $ assertEqual "leEqPR (S Z,S(S Z)) (S Z,S (S (S Z)))" (False) (leEqPR (S Z,S(S Z)) (S Z,S (S (S Z)))),
        --TestCase $ assertEqual "leEqPR (Z,S Z) (S Z,S (S (S Z)))" (True) (leEqPR (Z,S Z) (S Z,S (S (S Z)))),
        --TestCase $ assertEqual "leEqPR (S Z,S (S (S Z))) (S Z,S Z)" (True) (leEqPR (S Z,S (S (S Z))) (S Z,S Z))
        ]
    )

equalPR :: PosRat -> PosRat -> Bool
equalPR x y = equalNat m1 m2 && equalNat n1 n2
    where
        m1 = fst x
        n1 = snd x
        m2 = fst y
        n2 = snd y

equalNat :: Nat -> Nat -> Bool
equalNat x y = a == b
      where a = natToInt x
            b = natToInt y

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n
