-- Funktionale Programmierung WS2014
-- Exercise: 7
-- File: HUnit7.hs
-- HUnit test cases

import Aufgabe7
import Test.HUnit


main = runTestTT $ TestList [test1,test2,test3] --,test5,test6]




test1 = TestLabel "Test: get_spm" $ (
    TestList [
        TestCase $ assertEqual "get_spm h1" (r1) (get_spm h1)
    ]
    )

test2 = TestLabel "Test: get_mdhm" $ (
    TestList [
        TestCase $ assertEqual "get_mdhm h1" (r2) (get_mdhm h1)
    ]
    )

test3 = TestLabel "Test: get_mdhi" $ (
    TestList [
        TestCase $ assertEqual "get_mdhi h1" (r3) (get_mdhi h1)
    ]
    )

test4 = TestLabel "Test: get_pv" $ (
    TestList [

    ]
    )

test5 = TestLabel "Test: get_ugr" $ (
    TestList [
        TestCase $ assertEqual "get_ugr h1" (r5) (get_ugr h1)
    ]
    )

test6 = TestLabel "Test: get_tsp" $ (
    TestList [
        TestCase $ assertEqual "get_tsp h1" (r6) (get_tsp h1)
    ]
    )




natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

intToNat :: Int -> Nat
intToNat 0 = Z
intToNat i = S $ intToNat $ i-1 

-- Punkte Saison 1
p1s1 :: Verein -> Nat
p1s1 Sturm = intToNat 10
p1s1 WAC = intToNat 9
p1s1 Austria = intToNat 8
p1s1 WrNeustadt = intToNat 7
p1s1 RBSbg = intToNat 6
p1s1 Groedig = intToNat 5
p1s1 Rapid = intToNat 4
p1s1 Admira = intToNat 3
p1s1 Ried = intToNat 2
p1s1 Altach = intToNat 1

-- Punkte Saison 2
p1s2 :: Verein -> Nat
p1s2 Sturm = intToNat 10
p1s2 WAC = intToNat 9
p1s2 Austria = intToNat 8
p1s2 WrNeustadt = intToNat 7
p1s2 RBSbg = intToNat 6
p1s2 Groedig = intToNat 5
p1s2 Rapid = intToNat 4
p1s2 Admira = intToNat 3
p1s2 Ried = intToNat 2
p1s2 Altach = intToNat 1

-- Punkte Saison 3
p1s3 :: Verein -> Nat
p1s3 Sturm = intToNat 9
p1s3 WAC = intToNat 10
p1s3 Austria = intToNat 8
p1s3 WrNeustadt = intToNat 7
p1s3 RBSbg = intToNat 6
p1s3 Groedig = intToNat 5
p1s3 Rapid = intToNat 4
p1s3 Admira = intToNat 3
p1s3 Ried = intToNat 2
p1s3 Altach = intToNat 1

-- Trainer Saison 1
t1s1 :: Verein -> TrainerId
t1s1 Sturm = TId $ intToNat 10
t1s1 WAC = TId $ intToNat 9
t1s1 Austria = TId $ intToNat 8
t1s1 WrNeustadt = TId $ intToNat 7
t1s1 RBSbg = TId $ intToNat 6
t1s1 Groedig = TId $ intToNat 5
t1s1 Rapid = TId $ intToNat 4
t1s1 Admira = TId $ intToNat 3
t1s1 Ried = TId $ intToNat 2
t1s1 Altach = TId $ intToNat 1

-- Trainer Saison 2
t1s2 :: Verein -> TrainerId
t1s2 t = t1s1 t

-- Trainer Saison 3
t1s3 :: Verein -> TrainerId
t1s3 t = t1s1 t

-- Kader Saison 1
k1s1 :: Verein -> [SpielerId]
k1s1 Sturm = [SId $ intToNat 1,SId $ intToNat 2,SId $ intToNat 3,SId $ intToNat 4]
k1s1 WAC = [SId $ intToNat 5,SId $ intToNat 6,SId $ intToNat 7,SId $ intToNat 8]
k1s1 Austria = [SId $ intToNat 9,SId $ intToNat 10,SId $ intToNat 11,SId $ intToNat 12]
k1s1 WrNeustadt = [SId $ intToNat 13,SId $ intToNat 14,SId $ intToNat 15,SId $ intToNat 16]
k1s1 RBSbg = [SId $ intToNat 17,SId $ intToNat 18,SId $ intToNat 19,SId $ intToNat 20]
k1s1 Groedig = [SId $ intToNat 21,SId $ intToNat 22,SId $ intToNat 23,SId $ intToNat 24]
k1s1 Rapid = [SId $ intToNat 25,SId $ intToNat 26,SId $ intToNat 27,SId $ intToNat 28]
k1s1 Admira = [SId $ intToNat 29,SId $ intToNat 30,SId $ intToNat 31,SId $ intToNat 32]
k1s1 Ried = [SId $ intToNat 33,SId $ intToNat 34,SId $ intToNat 35,SId $ intToNat 36]
k1s1 Altach = [SId $ intToNat 37,SId $ intToNat 38,SId $ intToNat 39,SId $ intToNat 40]

-- Kader Saison 2
k1s2 :: Verein -> [SpielerId]
k1s2 Sturm = [SId $ intToNat 1,SId $ intToNat 2,SId $ intToNat 3,SId $ intToNat 4]
k1s2 WAC = [SId $ intToNat 5,SId $ intToNat 6,SId $ intToNat 7,SId $ intToNat 8]
k1s2 Austria = [SId $ intToNat 9,SId $ intToNat 10,SId $ intToNat 11,SId $ intToNat 12]
k1s2 WrNeustadt = [SId $ intToNat 13,SId $ intToNat 14,SId $ intToNat 15,SId $ intToNat 16]
k1s2 RBSbg = [SId $ intToNat 17,SId $ intToNat 18,SId $ intToNat 19,SId $ intToNat 20]
k1s2 Groedig = [SId $ intToNat 21,SId $ intToNat 22,SId $ intToNat 23,SId $ intToNat 24]
k1s2 Rapid = [SId $ intToNat 25,SId $ intToNat 26,SId $ intToNat 27,SId $ intToNat 28]
k1s2 Admira = [SId $ intToNat 29,SId $ intToNat 30,SId $ intToNat 31,SId $ intToNat 32]
k1s2 Ried = [SId $ intToNat 33,SId $ intToNat 34,SId $ intToNat 35,SId $ intToNat 36]
k1s2 Altach = [SId $ intToNat 37,SId $ intToNat 38,SId $ intToNat 39,SId $ intToNat 40]

-- Kader Saison 3
k1s3 :: Verein -> [SpielerId]
k1s3 Sturm = [SId $ intToNat 5,SId $ intToNat 6,SId $ intToNat 7,SId $ intToNat 4]
k1s3 WAC = [SId $ intToNat 1,SId $ intToNat 2,SId $ intToNat 3,SId $ intToNat 8]
k1s3 Austria = [SId $ intToNat 9,SId $ intToNat 10,SId $ intToNat 11,SId $ intToNat 12]
k1s3 WrNeustadt = [SId $ intToNat 13,SId $ intToNat 14,SId $ intToNat 15,SId $ intToNat 16]
k1s3 RBSbg = [SId $ intToNat 17,SId $ intToNat 18,SId $ intToNat 19,SId $ intToNat 20]
k1s3 Groedig = [SId $ intToNat 21,SId $ intToNat 22,SId $ intToNat 23,SId $ intToNat 24]
k1s3 Rapid = [SId $ intToNat 25,SId $ intToNat 26,SId $ intToNat 27,SId $ intToNat 28]
k1s3 Admira = [SId $ intToNat 29,SId $ intToNat 30,SId $ intToNat 31,SId $ intToNat 32]
k1s3 Ried = [SId $ intToNat 33,SId $ intToNat 34,SId $ intToNat 35,SId $ intToNat 36]
k1s3 Altach = [SId $ intToNat 37,SId $ intToNat 38,SId $ intToNat 39,SId $ intToNat 40]

--  Saison 1 - 3
s1 = (p1s1, Kd k1s1, Tr t1s1)
s2 = (p1s2, Kd k1s2, Tr t1s2)
s3 = (p1s3, Kd k1s3, Tr t1s3)

-- Historie
h1 = [s1,s2,s3]

-- Resultat Aufgabe 1
r1 = [SId $ intToNat 1,SId $ intToNat 2,SId $ intToNat 3]

-- Resultat Aufgabe 2
r2 = [Austria,RBSbg,Rapid]

-- Resultat Aufgabe 3
r3 = [Sturm,WAC,WrNeustadt,Admira]

-- Resultat Aufgabe 5
r5 = [SId $ intToNat 40,SId $ intToNat 39,SId $ intToNat 38,SId $ intToNat 37]

-- Resultat Aufgabe 6
r6 = [TId $ intToNat 8,TId $ intToNat 9,TId $ intToNat 10]