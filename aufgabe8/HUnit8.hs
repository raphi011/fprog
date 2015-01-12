-- Funktionale Programmierung WS2014
-- Exercise: 8
-- File: HUnit8.hs
-- HUnit test cases

import Aufgabe8
import Test.HUnit


main = runTestTT $ TestList [test1,test2,test3,test4,test5] --,test6,test7,test8,test9]

test1 = TestLabel "Test: isValidBudget" $ (
    TestList [
        TestCase $ assertEqual "isValidBudget b1" (True) (isValidBudget b1),
        TestCase $ assertEqual "isValidBudget b2" (True) (isValidBudget b2),
        TestCase $ assertEqual "isValidBudget b3" (True) (isValidBudget b3),
        TestCase $ assertEqual "isValidBudget b4" (False) (isValidBudget b4)
    ]
    )

test2 = TestLabel "Test: isValidSpiel" $ (
    TestList [
        TestCase $ assertEqual "isValidSpiel s1" (False) (isValidSpiel s1),
        TestCase $ assertEqual "isValidSpiel s2" (True) (isValidSpiel s2),
        TestCase $ assertEqual "isValidSpiel s3" (True) (isValidSpiel s3),
        TestCase $ assertEqual "isValidSpiel s4" (True) (isValidSpiel s4),
        TestCase $ assertEqual "isValidSpiel s5" (True) (isValidSpiel s5),
        TestCase $ assertEqual "isValidSpiel s6" (True) (isValidSpiel s6),
        TestCase $ assertEqual "isValidSpiel s7" (True) (isValidSpiel s7),
        TestCase $ assertEqual "isValidSpiel s8" (True) (isValidSpiel s8),
        TestCase $ assertEqual "isValidSpiel s9" (True) (isValidSpiel s9),
        TestCase $ assertEqual "isValidSpiel s10" (True) (isValidSpiel s10),
        TestCase $ assertEqual "isValidSpiel s11" (True) (isValidSpiel s11),
        TestCase $ assertEqual "isValidSpiel s12" (False) (isValidSpiel s12),
        TestCase $ assertEqual "isValidSpiel s13" (True) (isValidSpiel s13),
        TestCase $ assertEqual "isValidSpiel s14" (True) (isValidSpiel s14),
        TestCase $ assertEqual "isValidSpiel s15" (True) (isValidSpiel s15),
        TestCase $ assertEqual "isValidSpiel s16" (True) (isValidSpiel s16),
        TestCase $ assertEqual "isValidSpiel s17" (True) (isValidSpiel s17),
        TestCase $ assertEqual "isValidSpiel s18" (True) (isValidSpiel s18),
        TestCase $ assertEqual "isValidSpiel s19" (True) (isValidSpiel s19),
        TestCase $ assertEqual "isValidSpiel s20" (True) (isValidSpiel s20),
        TestCase $ assertEqual "isValidSpiel s21" (True) (isValidSpiel s21),
        TestCase $ assertEqual "isValidSpiel s22" (True) (isValidSpiel s22),
        TestCase $ assertEqual "isValidSpiel s23" (False) (isValidSpiel s23),
        TestCase $ assertEqual "isValidSpiel s24" (True) (isValidSpiel s24),
        TestCase $ assertEqual "isValidSpiel s25" (True) (isValidSpiel s25),
        TestCase $ assertEqual "isValidSpiel s26" (True) (isValidSpiel s26),
        TestCase $ assertEqual "isValidSpiel s27" (True) (isValidSpiel s27),
        TestCase $ assertEqual "isValidSpiel s28" (True) (isValidSpiel s28),
        TestCase $ assertEqual "isValidSpiel s29" (True) (isValidSpiel s29),
        TestCase $ assertEqual "isValidSpiel s30" (True) (isValidSpiel s30),
        TestCase $ assertEqual "isValidSpiel s31" (True) (isValidSpiel s31),
        TestCase $ assertEqual "isValidSpiel s32" (True) (isValidSpiel s32),
        TestCase $ assertEqual "isValidSpiel s33" (True) (isValidSpiel s33),
        TestCase $ assertEqual "isValidSpiel s34" (False) (isValidSpiel s34),
        TestCase $ assertEqual "isValidSpiel s35" (True) (isValidSpiel s35),
        TestCase $ assertEqual "isValidSpiel s36" (True) (isValidSpiel s36),
        TestCase $ assertEqual "isValidSpiel s37" (True) (isValidSpiel s37),
        TestCase $ assertEqual "isValidSpiel s38" (True) (isValidSpiel s38),
        TestCase $ assertEqual "isValidSpiel s39" (True) (isValidSpiel s39),
        TestCase $ assertEqual "isValidSpiel s40" (True) (isValidSpiel s40),
        TestCase $ assertEqual "isValidSpiel s41" (True) (isValidSpiel s41),
        TestCase $ assertEqual "isValidSpiel s42" (True) (isValidSpiel s42),
        TestCase $ assertEqual "isValidSpiel s43" (True) (isValidSpiel s43),
        TestCase $ assertEqual "isValidSpiel s44" (True) (isValidSpiel s44),
        TestCase $ assertEqual "isValidSpiel s45" (False) (isValidSpiel s45),
        TestCase $ assertEqual "isValidSpiel s46" (True) (isValidSpiel s46),
        TestCase $ assertEqual "isValidSpiel s47" (True) (isValidSpiel s47),
        TestCase $ assertEqual "isValidSpiel s48" (True) (isValidSpiel s48),
        TestCase $ assertEqual "isValidSpiel s49" (True) (isValidSpiel s49),
        TestCase $ assertEqual "isValidSpiel s50" (True) (isValidSpiel s50),
        TestCase $ assertEqual "isValidSpiel s51" (True) (isValidSpiel s51),
        TestCase $ assertEqual "isValidSpiel s52" (True) (isValidSpiel s52),
        TestCase $ assertEqual "isValidSpiel s53" (True) (isValidSpiel s53),
        TestCase $ assertEqual "isValidSpiel s54" (True) (isValidSpiel s54),
        TestCase $ assertEqual "isValidSpiel s55" (True) (isValidSpiel s55),
        TestCase $ assertEqual "isValidSpiel s56" (False) (isValidSpiel s56),
        TestCase $ assertEqual "isValidSpiel s57" (True) (isValidSpiel s57),
        TestCase $ assertEqual "isValidSpiel s58" (True) (isValidSpiel s58),
        TestCase $ assertEqual "isValidSpiel s59" (True) (isValidSpiel s59),
        TestCase $ assertEqual "isValidSpiel s60" (True) (isValidSpiel s60),
        TestCase $ assertEqual "isValidSpiel s61" (True) (isValidSpiel s61),
        TestCase $ assertEqual "isValidSpiel s62" (True) (isValidSpiel s62),
        TestCase $ assertEqual "isValidSpiel s63" (True) (isValidSpiel s63),
        TestCase $ assertEqual "isValidSpiel s64" (True) (isValidSpiel s64),
        TestCase $ assertEqual "isValidSpiel s65" (True) (isValidSpiel s65),
        TestCase $ assertEqual "isValidSpiel s66" (True) (isValidSpiel s66),
        TestCase $ assertEqual "isValidSpiel s67" (False) (isValidSpiel s67),
        TestCase $ assertEqual "isValidSpiel s68" (True) (isValidSpiel s68),
        TestCase $ assertEqual "isValidSpiel s69" (True) (isValidSpiel s69),
        TestCase $ assertEqual "isValidSpiel s70" (True) (isValidSpiel s70),
        TestCase $ assertEqual "isValidSpiel s71" (True) (isValidSpiel s71),
        TestCase $ assertEqual "isValidSpiel s72" (True) (isValidSpiel s72),
        TestCase $ assertEqual "isValidSpiel s73" (True) (isValidSpiel s73),
        TestCase $ assertEqual "isValidSpiel s74" (True) (isValidSpiel s74),
        TestCase $ assertEqual "isValidSpiel s75" (True) (isValidSpiel s75),
        TestCase $ assertEqual "isValidSpiel s76" (True) (isValidSpiel s76),
        TestCase $ assertEqual "isValidSpiel s77" (True) (isValidSpiel s77),
        TestCase $ assertEqual "isValidSpiel s78" (False) (isValidSpiel s78),
        TestCase $ assertEqual "isValidSpiel s79" (True) (isValidSpiel s79),
        TestCase $ assertEqual "isValidSpiel s80" (True) (isValidSpiel s80),
        TestCase $ assertEqual "isValidSpiel s81" (True) (isValidSpiel s81),
        TestCase $ assertEqual "isValidSpiel s82" (True) (isValidSpiel s82),
        TestCase $ assertEqual "isValidSpiel s83" (True) (isValidSpiel s83),
        TestCase $ assertEqual "isValidSpiel s84" (True) (isValidSpiel s84),
        TestCase $ assertEqual "isValidSpiel s85" (True) (isValidSpiel s85),
        TestCase $ assertEqual "isValidSpiel s86" (True) (isValidSpiel s86),
        TestCase $ assertEqual "isValidSpiel s87" (True) (isValidSpiel s87),
        TestCase $ assertEqual "isValidSpiel s88" (True) (isValidSpiel s88),
        TestCase $ assertEqual "isValidSpiel s89" (False) (isValidSpiel s89),
        TestCase $ assertEqual "isValidSpiel s90" (True) (isValidSpiel s90),
        TestCase $ assertEqual "isValidSpiel s91" (True) (isValidSpiel s91),
        TestCase $ assertEqual "isValidSpiel s92" (True) (isValidSpiel s92),
        TestCase $ assertEqual "isValidSpiel s93" (True) (isValidSpiel s93),
        TestCase $ assertEqual "isValidSpiel s94" (True) (isValidSpiel s94),
        TestCase $ assertEqual "isValidSpiel s95" (True) (isValidSpiel s95),
        TestCase $ assertEqual "isValidSpiel s96" (True) (isValidSpiel s96),
        TestCase $ assertEqual "isValidSpiel s97" (True) (isValidSpiel s97),
        TestCase $ assertEqual "isValidSpiel s98" (True) (isValidSpiel s98),
        TestCase $ assertEqual "isValidSpiel s99" (True) (isValidSpiel s99),
        TestCase $ assertEqual "isValidSpiel s100" (False) (isValidSpiel s100)
    ]
    )

test3 = TestLabel "Test: isValidSpieltag" $ (
    TestList [
        TestCase $ assertEqual "isValidSpieltag st1" (False) (isValidSpieltag st1),
        TestCase $ assertEqual "isValidSpieltag st2" (False) (isValidSpieltag st2),
        TestCase $ assertEqual "isValidSpieltag st3" (False) (isValidSpieltag st3),
        TestCase $ assertEqual "isValidSpieltag st4" (False) (isValidSpieltag st4),
        TestCase $ assertEqual "isValidSpieltag st5" (True) (isValidSpieltag st5),
        TestCase $ assertEqual "isValidSpieltag st6" (True) (isValidSpieltag st6),
        TestCase $ assertEqual "isValidSpieltag st7" (True) (isValidSpieltag st7),
        TestCase $ assertEqual "isValidSpieltag st8" (True) (isValidSpieltag st8),
        TestCase $ assertEqual "isValidSpieltag st9" (True) (isValidSpieltag st9)
    ]
    )

test4 = TestLabel "Test: isValidRestprogramm" $ (
    TestList [
        TestCase $ assertEqual "isValidRestprogramm rp1" (False) (isValidRestprogramm rp1),
        TestCase $ assertEqual "isValidRestprogramm rp2" (False) (isValidRestprogramm rp2),
        TestCase $ assertEqual "isValidRestprogramm rp3" (False) (isValidRestprogramm rp3),
        TestCase $ assertEqual "isValidRestprogramm rp4" (True) (isValidRestprogramm rp4)
    ]
    )

test5 = TestLabel "Test: mkTabelle" $ (
    TestList [
        TestCase $ assertEqual "mkTabelle p1 b1" (rMT1) (mkTabelle p1 b1),
        TestCase $ assertEqual "mkTabelle p2 b1" (rMT4) (mkTabelle p2 b1),
        TestCase $ assertEqual "mkTabelle p3 b1" (rMT5) (mkTabelle p3 b1),
        TestCase $ assertEqual "mkTabelle p1 b2" (rMT2) (mkTabelle p1 b2),
        TestCase $ assertEqual "mkTabelle p2 b2" (rMT4) (mkTabelle p2 b2),
        TestCase $ assertEqual "mkTabelle p3 b2" (rMT4) (mkTabelle p3 b2),
        TestCase $ assertEqual "mkTabelle p1 b3" (rMT3) (mkTabelle p1 b3),
        TestCase $ assertEqual "mkTabelle p2 b3" (rMT4) (mkTabelle p2 b3),
        TestCase $ assertEqual "mkTabelle p3 b3" (rMT4) (mkTabelle p3 b3),
        TestCase $ assertEqual "mkTabelle p1 b4" (rMTE) (mkTabelle p1 b4), -- ERROR expected
        TestCase $ assertEqual "mkTabelle p2 b4" (rMTE) (mkTabelle p2 b4), -- ERROR expected
        TestCase $ assertEqual "mkTabelle p3 b4" (rMTE) (mkTabelle p3 b4) -- ERROR expected
    ]
    )

test6 = TestLabel "Test: hm_fix" $ (
    TestList [
    ]
    )

test7 = TestLabel "Test: hm_ausEigenerKraft" $ (
    TestList [
    ]
    )

test8 = TestLabel "Test: hm_alleMitRechnerischerChance" $ (
    TestList [
    ]
    )

test9 = TestLabel "Test: vhm_alleMitRechnerischerChance" $ (
    TestList [
    ]
    )




natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

intToNat :: Int -> Nat
intToNat 0 = Z
intToNat i = S $ intToNat $ i-1 


-- Punkte
p1 :: Verein -> Nat
p1 Sturm = intToNat 100
p1 WAC = intToNat 100
p1 Austria = intToNat 100
p1 WrNeustadt = intToNat 100
p1 RBSbg = intToNat 100
p1 Groedig = intToNat 100
p1 Rapid = intToNat 100
p1 Admira = intToNat 100
p1 Ried = intToNat 100
p1 Altach = intToNat 100

p2 :: Verein -> Nat
p2 Sturm = intToNat 90
p2 WAC = intToNat 17
p2 Austria = intToNat 10
p2 WrNeustadt = intToNat 100
p2 RBSbg = intToNat 85
p2 Groedig = intToNat 50
p2 Rapid = intToNat 64
p2 Admira = intToNat 84
p2 Ried = intToNat 12
p2 Altach = intToNat 95

p3 :: Verein -> Nat
p3 Sturm = intToNat 90
p3 WAC = intToNat 17
p3 Austria = intToNat 10
p3 WrNeustadt = intToNat 100
p3 RBSbg = intToNat 85
p3 Groedig = intToNat 50
p3 Rapid = intToNat 64
p3 Admira = intToNat 84
p3 Ried = intToNat 12
p3 Altach = intToNat 100


-- Budget
b1 :: Verein -> Nat
b1 Sturm = intToNat 10
b1 WAC = intToNat 9
b1 Austria = intToNat 8
b1 WrNeustadt = intToNat 7
b1 RBSbg = intToNat 6
b1 Groedig = intToNat 5
b1 Rapid = intToNat 4
b1 Admira = intToNat 3
b1 Ried = intToNat 2
b1 Altach = intToNat 1

b2 :: Verein -> Nat
b2 Sturm = intToNat 1
b2 WAC = intToNat 2
b2 Austria = intToNat 3
b2 WrNeustadt = intToNat 4
b2 RBSbg = intToNat 5
b2 Groedig = intToNat 6
b2 Rapid = intToNat 7
b2 Admira = intToNat 8
b2 Ried = intToNat 9
b2 Altach = intToNat 10

b3 :: Verein -> Nat
b3 Sturm = intToNat 4
b3 WAC = intToNat 3
b3 Austria = intToNat 5
b3 WrNeustadt = intToNat 2
b3 RBSbg = intToNat 10
b3 Groedig = intToNat 1
b3 Rapid = intToNat 6
b3 Admira = intToNat 8
b3 Ried = intToNat 7
b3 Altach = intToNat 9

b4 :: Verein -> Nat
b4 Sturm = intToNat 10
b4 WAC = intToNat 10
b4 Austria = intToNat 10
b4 WrNeustadt = intToNat 10
b4 RBSbg = intToNat 10
b4 Groedig = intToNat 10
b4 Rapid = intToNat 10
b4 Admira = intToNat 10
b4 Ried = intToNat 10
b4 Altach = intToNat 10


--Restprogramme
rp1 = (Rp st1 st1 st1)
rp2 = (Rp st1 st1 st5)
rp3 = (Rp st5 st6 st7)
rp4 = (Rp st5 st8 st9)


-- Spieltage
st1 = St (s1,s1,s1,s1,s1)
st2 = St (s1,s1,s1,s1,s2)
st3 = St (s1,s2,s3,s4,s5)
st4 = St (s2,s3,s4,s5,s6)
st5 = St (s2,s24,s46,s68,s90)
st6 = St (s3,s14,s46,s68,s90)
st7 = St (s4,s13,s46,s68,s90)
st8 = St (s5,s16,s27,s38,s99)
st9 = St (s6,s15,s28,s39,s70)


-- Spiele
s1 = (Sturm,Sturm)
s2 = (Sturm,WAC)
s3 = (Sturm,Austria)
s4 = (Sturm,WrNeustadt)
s5 = (Sturm,RBSbg)
s6 = (Sturm,Groedig)
s7 = (Sturm,Rapid)
s8 = (Sturm,Admira)
s9 = (Sturm,Ried)
s10 = (Sturm,Altach)
s11 = (WAC,Sturm)
s12 = (WAC,WAC)
s13 = (WAC,Austria)
s14 = (WAC,WrNeustadt)
s15 = (WAC,RBSbg)
s16 = (WAC,Groedig)
s17 = (WAC,Rapid)
s18 = (WAC,Admira)
s19 = (WAC,Ried)
s20 = (WAC,Altach)
s21 = (Austria,Sturm)
s22 = (Austria,WAC)
s23 = (Austria,Austria)
s24 = (Austria,WrNeustadt)
s25 = (Austria,RBSbg)
s26 = (Austria,Groedig)
s27 = (Austria,Rapid)
s28 = (Austria,Admira)
s29 = (Austria,Ried)
s30 = (Austria,Altach)
s31 = (WrNeustadt,Sturm)
s32 = (WrNeustadt,WAC)
s33 = (WrNeustadt,Austria)
s34 = (WrNeustadt,WrNeustadt)
s35 = (WrNeustadt,RBSbg)
s36 = (WrNeustadt,Groedig)
s37 = (WrNeustadt,Rapid)
s38 = (WrNeustadt,Admira)
s39 = (WrNeustadt,Ried)
s40 = (WrNeustadt,Altach)
s41 = (RBSbg,Sturm)
s42 = (RBSbg,WAC)
s43 = (RBSbg,Austria)
s44 = (RBSbg,WrNeustadt)
s45 = (RBSbg,RBSbg)
s46 = (RBSbg,Groedig)
s47 = (RBSbg,Rapid)
s48 = (RBSbg,Admira)
s49 = (RBSbg,Ried)
s50 = (RBSbg,Altach)
s51 = (Groedig,Sturm)
s52 = (Groedig,WAC)
s53 = (Groedig,Austria)
s54 = (Groedig,WrNeustadt)
s55 = (Groedig,RBSbg)
s56 = (Groedig,Groedig)
s57 = (Groedig,Rapid)
s58 = (Groedig,Admira)
s59 = (Groedig,Ried)
s60 = (Groedig,Altach)
s61 = (Rapid,Sturm)
s62 = (Rapid,WAC)
s63 = (Rapid,Austria)
s64 = (Rapid,WrNeustadt)
s65 = (Rapid,RBSbg)
s66 = (Rapid,Groedig)
s67 = (Rapid,Rapid)
s68 = (Rapid,Admira)
s69 = (Rapid,Ried)
s70 = (Rapid,Altach)
s71 = (Admira,Sturm)
s72 = (Admira,WAC)
s73 = (Admira,Austria)
s74 = (Admira,WrNeustadt)
s75 = (Admira,RBSbg)
s76 = (Admira,Groedig)
s77 = (Admira,Rapid)
s78 = (Admira,Admira)
s79 = (Admira,Ried)
s80 = (Admira,Altach)
s81 = (Ried,Sturm)
s82 = (Ried,WAC)
s83 = (Ried,Austria)
s84 = (Ried,WrNeustadt)
s85 = (Ried,RBSbg)
s86 = (Ried,Groedig)
s87 = (Ried,Rapid)
s88 = (Ried,Admira)
s89 = (Ried,Ried)
s90 = (Ried,Altach)
s91 = (Altach,Sturm)
s92 = (Altach,WAC)
s93 = (Altach,Austria)
s94 = (Altach,WrNeustadt)
s95 = (Altach,RBSbg)
s96 = (Altach,Groedig)
s97 = (Altach,Rapid)
s98 = (Altach,Admira)
s99 = (Altach,Ried)
s100 = (Altach,Altach)


-- Resultat mkTabelle
rMT1 = [Altach,Ried,Admira,Rapid,Groedig,RBSbg,WrNeustadt,Austria,WAC,Sturm]
rMT2 = [Sturm,WAC,Austria,WrNeustadt,RBSbg,Groedig,Rapid,Admira,Ried,Altach]
rMT3 = [Groedig,WrNeustadt,WAC,Sturm,Austria,Rapid,Ried,Admira,Altach,RBSbg]
rMT4 = [WrNeustadt,Altach,Sturm,RBSbg,Admira,Rapid,Groedig,WAC,Ried,Austria]
rMT5 = [Altach,WrNeustadt,Sturm,RBSbg,Admira,Rapid,Groedig,WAC,Ried,Austria]
rMTE = []
