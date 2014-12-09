-- Funktionale Programmierung WS2014
-- Exercise: 7
-- File: HUnit7.hs
-- HUnit test cases


import Aufgabe7
import Test.HUnit


main = runTestTT $ TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10]


test1 = TestLabel "Test: get_spm" $ (
  TestList [
    TestCase $ assertEqual "get_spm h1" (r11) (get_spm h1),
    TestCase $ assertEqual "get_spm h2" (r12) (get_spm h2),
    TestCase $ assertEqual "get_spm h3" (r13) (get_spm h3),
    TestCase $ assertEqual "get_spm h4" (r14) (get_spm h4),
    TestCase $ assertEqual "get_spm h5" (r15) (get_spm h5),
    TestCase $ assertEqual "get_spm h6" (r16) (get_spm h6),
    TestCase $ assertEqual "get_spm h7" (r17) (get_spm h7)
    ]
    )


test2 = TestLabel "Test: get_mdhm" $ (
    TestList [
    TestCase $ assertEqual "get_mdhm h1" (r21) (get_mdhm h1),
    TestCase $ assertEqual "get_mdhm h2" (r22) (get_mdhm h2),
    TestCase $ assertEqual "get_mdhm h3" (r23) (get_mdhm h3),
    TestCase $ assertEqual "get_mdhm h4" (r24) (get_mdhm h4),
    TestCase $ assertEqual "get_mdhm h5" (r25) (get_mdhm h5),
    TestCase $ assertEqual "get_mdhm h6" (r26) (get_mdhm h6),
    TestCase $ assertEqual "get_mdhm h7" (r27) (get_mdhm h7)
    ]
    )


test3 = TestLabel "Test: get_mdhi" $ (
    TestList [
    TestCase $ assertEqual "get_mdhi h1" (r31) (get_mdhi h1),
    TestCase $ assertEqual "get_mdhi h2" (r32) (get_mdhi h2),
    TestCase $ assertEqual "get_mdhi h3" (r33) (get_mdhi h3),
    TestCase $ assertEqual "get_mdhi h4" (r34) (get_mdhi h4),
    TestCase $ assertEqual "get_mdhi h5" (r35) (get_mdhi h5),
    TestCase $ assertEqual "get_mdhi h6" (r36) (get_mdhi h6),
    TestCase $ assertEqual "get_mdhi h7" (r37) (get_mdhi h7)
    ]
    )


test4 = TestLabel "Test: get_pv" $ (
    TestList [
    TestCase $ assertEqual "get_pv h1" (r41) (get_pv h1),
    TestCase $ assertEqual "get_pv h2" (r42) (get_pv h2),
    TestCase $ assertEqual "get_pv h3" (r43) (get_pv h3),
    TestCase $ assertEqual "get_pv h4" (r44) (get_pv h4),
    TestCase $ assertEqual "get_pv h5" (r45) (get_pv h5),
    TestCase $ assertEqual "get_pv h6" (r46) (get_pv h6),
    TestCase $ assertEqual "get_pv h7" (r47) (get_pv h7)
    ]
    )


test5 = TestLabel "Test: get_ugr" $ (
    TestList [
    TestCase $ assertEqual "get_ugr h1" (r51) (get_ugr h1),
    TestCase $ assertEqual "get_ugr h2" (r52) (get_ugr h2),
    TestCase $ assertEqual "get_ugr h3" (r53) (get_ugr h3),
    TestCase $ assertEqual "get_ugr h4" (r54) (get_ugr h4),
    TestCase $ assertEqual "get_ugr h5" (r55) (get_ugr h5),
    TestCase $ assertEqual "get_ugr h6" (r56) (get_ugr h6),
    TestCase $ assertEqual "get_ugr h7" (r57) (get_ugr h7)
    ]
    )


test6 = TestLabel "Test: get_tsp" $ (
    TestList [
    TestCase $ assertEqual "get_tsp h1" (r61) (get_tsp h1),
    TestCase $ assertEqual "get_tsp h2" (r62) (get_tsp h2),
    TestCase $ assertEqual "get_tsp h3" (r63) (get_tsp h3),
    TestCase $ assertEqual "get_tsp h4" (r64) (get_tsp h4),
    TestCase $ assertEqual "get_tsp h5" (r65) (get_tsp h5),
    TestCase $ assertEqual "get_tsp h6" (r66) (get_tsp h6),
    TestCase $ assertEqual "get_tsp h7" (r67) (get_tsp h7)
    ]
    )


test7 = TestLabel "Test: get_taz" $ (
    TestList [
       TestCase $ assertEqual "get_taz h1" (r71) (get_taz h1),
       TestCase $ assertEqual "get_taz h2" (r72) (get_taz h2),
       TestCase $ assertEqual "get_taz h3" (r73) (get_taz h3),
       TestCase $ assertEqual "get_taz h4" (r74) (get_taz h4),
       TestCase $ assertEqual "get_taz h5" (r75) (get_taz h5),
       TestCase $ assertEqual "get_taz h6" (r76) (get_taz h6),
       TestCase $ assertEqual "get_taz h7" (r77) (get_taz h7)
    ]
    )


test8 = TestLabel "Test: get_vsz" $ (
    TestList [
       TestCase $ assertEqual "get_vsz h1" (r81) (get_vsz h1),
       TestCase $ assertEqual "get_vsz h2" (r82) (get_vsz h2),
       TestCase $ assertEqual "get_vsz h3" (r83) (get_vsz h3),
       TestCase $ assertEqual "get_vsz h4" (r84) (get_vsz h4),
       TestCase $ assertEqual "get_vsz h5" (r85) (get_vsz h5),
       TestCase $ assertEqual "get_vsz h6" (r86) (get_vsz h6),
       TestCase $ assertEqual "get_vsz h7" (r87) (get_vsz h7)
    ]
    )


test9 = TestLabel "Test: get_tmv" $ (
    TestList [
       TestCase $ assertEqual "get_tmv h1" (r91) (get_tmv h1),
       TestCase $ assertEqual "get_tmv h2" (r92) (get_tmv h2),
       TestCase $ assertEqual "get_tmv h3" (r93) (get_tmv h3),
       TestCase $ assertEqual "get_tmv h4" (r94) (get_tmv h4),
       TestCase $ assertEqual "get_tmv h5" (r95) (get_tmv h5),
       TestCase $ assertEqual "get_tmv h6" (r96) (get_tmv h6),
       TestCase $ assertEqual "get_tmv h7" (r97) (get_tmv h7)
    ]
    )


test10 = TestLabel "Test: get_tps" $ (
    TestList [


    ]
    )


toNat :: Integer -> Nat
toNat n
    | n < 0 = error "no negative numbers"
    | n == 0 = Z
    | otherwise = S (toNat (n-1))


toSId :: [Integer] -> [SpielerId]
toSId []     = []
toSId (x:xs) = [SId $ toNat x] ++ toSId xs


toTId :: [Integer] -> [TrainerId]
toTId []     = []
toTId (x:xs) = [TId $ toNat x] ++ toTId xs


-- Historie
h1 = [saison1]
h2 = [saison2]
h3 = [saison3]
h4 = [saison1,saison2]
h5 = [saison1,saison3]
h6 = [saison2,saison3]
h7 = [saison1,saison2,saison3]
-- Saisonen
saison1 = (p1, Kd k1, Tr tr1)
saison2 = (p2, Kd k2, Tr tr2)
saison3 = (p3, Kd k3, Tr tr3)
-- Spieler 
s1 =  SId $ toNat(1)
s2 =  SId $ toNat(2)
s3 =  SId $ toNat(3)
s4 =  SId $ toNat(4)
s5 =  SId $ toNat(5)
s6 =  SId $ toNat(6)
s7 =  SId $ toNat(7)
s8 =  SId $ toNat(8)
s9 =  SId $ toNat(9)
s10 = SId $ toNat(10)
s11 = SId $ toNat(11)
s12 = SId $ toNat(12)
s13 = SId $ toNat(13)
s14 = SId $ toNat(14)
s15 = SId $ toNat(15)
s16 = SId $ toNat(16)
s17 = SId $ toNat(17)
s18 = SId $ toNat(18)
s19 = SId $ toNat(19)
s20 = SId $ toNat(20)
s21 = SId $ toNat(21)
s22 = SId $ toNat(22)
s23 = SId $ toNat(23)
s24 = SId $ toNat(24)
s25 = SId $ toNat(25)
s26 = SId $ toNat(26)
s27 = SId $ toNat(27)
s28 = SId $ toNat(28)
s29 = SId $ toNat(29)
s30 = SId $ toNat(30)
s31 = SId $ toNat(31)
s32 = SId $ toNat(32)
s33 = SId $ toNat(33)
s34 = SId $ toNat(34)
s35 = SId $ toNat(35)
s36 = SId $ toNat(36)
s37 = SId $ toNat(37)
s38 = SId $ toNat(38)
s39 = SId $ toNat(39)
s40 = SId $ toNat(40)
s41 = SId $ toNat(41)
s42 = SId $ toNat(42)
s43 = SId $ toNat(43)
s44 = SId $ toNat(44)
s45 = SId $ toNat(45)
s46 = SId $ toNat(46)
s47 = SId $ toNat(47)
s48 = SId $ toNat(48)
s49 = SId $ toNat(49)
s50 = SId $ toNat(50)
-- Trainer
t1  = TId $ toNat (1)
t2  = TId $ toNat (2)
t3  = TId $ toNat (3)
t4  = TId $ toNat (4)
t5  = TId $ toNat (5)
t6  = TId $ toNat (6)
t7  = TId $ toNat (7)
t8  = TId $ toNat (8)
t9  = TId $ toNat (9)
t10 = TId $ toNat (10)


--------------------------------------------------------
--------------------- Saisonen -------------------------
--------------------------------------------------------


-------- Saison 1 --------
-- Kader
k1 x
  | x == Sturm       = [s1,s2,s3,s4,s5]
  | x == WAC         = [s6,s7,s8,s9,s10]
  | x == Austria     = [s11,s12,s13,s14,s15]
  | x == WrNeustadt  = [s16,s17,s18,s19,s20]
  | x == RBSbg       = [s21,s22,s23,s24,s25]
  | x == Groedig     = [s26,s27,s28,s29,s30]
  | x == Rapid       = [s31,s32,s33,s34,s35]
  | x == Admira      = [s36,s37,s38,s39,s40]
  | x == Ried        = [s41,s42,s43,s44,s45]
  | x == Altach      = [s46,s47,s48,s49,s50]
-- Trainer
tr1 x
  | x == Sturm      = t1
  | x == WAC        = t2
  | x == Austria    = t3
  | x == WrNeustadt = t4
  | x == RBSbg      = t5
  | x == Groedig    = t6
  | x == Rapid      = t7
  | x == Admira     = t8
  | x == Ried       = t9
  | x == Altach     = t10
-- Punkte
p1 x
  | x == Sturm      = toNat(20)
  | x == WAC        = toNat(10)
  | x == Austria    = toNat(18)
  | x == WrNeustadt = toNat(17)
  | x == RBSbg      = toNat(16)
  | x == Groedig    = toNat(16)
  | x == Rapid      = toNat(15)
  | x == Admira     = toNat(9)
  | x == Ried       = toNat(13)
  | x == Altach     = toNat(13)




-------- Saison 2 --------
-- Kader
k2 x
  | x == Sturm       = [s6,s2,s3,s29,s5]
  | x == WAC         = [s1,s7,s8,s9,s10]
  | x == Austria     = [s11,s12,s13,s14,s15]
  | x == WrNeustadt  = [s16,s17,s18,s19,s20]
  | x == RBSbg       = [s21,s22,s23,s24,s25]
  | x == Groedig     = [s26,s27,s28,s4,s30]
  | x == Rapid       = [s31,s32,s33,s34,s35]
  | x == Admira      = [s36,s37,s38,s39,s40]
  | x == Ried        = [s41,s42,s43,s44,s45]
  | x == Altach      = [s46,s47,s48,s49,s50]
-- Trainer
tr2 x
  | x == Sturm      = t2
  | x == WAC        = t1
  | x == Austria    = t3
  | x == WrNeustadt = t4
  | x == RBSbg      = t5
  | x == Groedig    = t6
  | x == Rapid      = t7
  | x == Admira     = t8
  | x == Ried       = t9
  | x == Altach     = t10
-- Punkte
p2 x
  | x == Sturm      = toNat(20)
  | x == WAC        = toNat(20)
  | x == Austria    = toNat(18)
  | x == WrNeustadt = toNat(17)
  | x == RBSbg      = toNat(16)
  | x == Groedig    = toNat(16)
  | x == Rapid      = toNat(15)
  | x == Admira     = toNat(14)
  | x == Ried       = toNat(13)
  | x == Altach     = toNat(8)


                      
-------- Saison 3 --------
-- Kader
k3 x
  | x == Sturm       = [s6,s12,s3,s4,s5]
  | x == WAC         = [s21,s7,s8,s9,s10]
  | x == Austria     = [s16,s2,s13,s14,s15]
  | x == WrNeustadt  = [s11,s27,s18,s19,s20]
  | x == RBSbg       = [s26,s22,s23,s24,s25]
  | x == Groedig     = [s1,s17,s28,s29,s30]
  | x == Rapid       = [s36,s32,s33,s34,s35]
  | x == Admira      = [s31,s47,s38,s39,s40]
  | x == Ried        = [s46,s42,s43,s44,s45]
  | x == Altach      = [s41,s37,s48,s49,s50]
-- Trainer
tr3 x
  | x == Sturm      = t3
  | x == WAC        = t2
  | x == Austria    = t1
  | x == WrNeustadt = t4
  | x == RBSbg      = t5
  | x == Groedig    = t6
  | x == Rapid      = t7
  | x == Admira     = t8
  | x == Ried       = t9
  | x == Altach     = t10
-- Punkte
p3 x
  | x == Sturm      = toNat(20)
  | x == WAC        = toNat(19)
  | x == Austria    = toNat(18)
  | x == WrNeustadt = toNat(17)
  | x == RBSbg      = toNat(16)
  | x == Groedig    = toNat(25)
  | x == Rapid      = toNat(15)
  | x == Admira     = toNat(14)
  | x == Ried       = toNat(13)
  | x == Altach     = toNat(13)


--------------------------------------------------------
--------------------- Results --------------------------
--------------------------------------------------------
-- Results get_spm
r11 = toSId [1,2,3,4,5]
r12 = toSId [2,3,5,6,29]
r13 = toSId [1,17,28,29,30]
r14 = toSId [1,2,3,4,5,6,29]
r15 = toSId [1]
r16 = toSId [29]
r17 = toSId [1,29]


--Results get_mdhm
r21 = [Austria,WrNeustadt,Groedig,Ried]
r22 = [WAC,Austria,RBSbg,Rapid]
r23 = [Sturm,WAC,WrNeustadt,Rapid]
r24 = [Austria]
r25 = [WrNeustadt]
r26 = [WAC,Rapid]
r27 = [WAC,Austria,WrNeustadt,Rapid]


--Results get_mdhi
r31 = [Sturm,Austria,RBSbg,Altach]
r32 = [Sturm,WAC,WrNeustadt,Admira]
r33 = [Sturm,Austria,Groedig,Admira]
r34 = [Sturm]
r35 = [Sturm,Austria]
r36 = [Sturm,Admira]
r37 = [Sturm]


--Results get_pv
r41 = toSId [15,14,13,12,11]
r42 = toSId [10,9,8,7,1]
r43 = toSId [12,6,5,4,3]
r44 = toSId [15,14,13,12,11,10,9,8,7]
r45 = toSId [12]
r46 = toSId [12,10,9,8,7,4]
r47 = toSId [12]


--Results get_ugr
r51 = toSId [40,39,38,37,36]
r52 = toSId [50,49,48,47,46]
r53 = toSId [50,49,48,41,37]
r54 = toSId [50,49,48,47,46,40,39,38,37,36]
r55 = toSId [37]
r56 = toSId [50,49,48]
r57 = toSId [50,49,48,37]


--Results get_tsp
r61 = toTId [1,3,4]
r62 = toTId [1,2,3]
r63 = toTId [2,3,6]
r64 = toTId [1,3]
r65 = toTId [3]
r66 = toTId [2,3]
r67 = toTId [3]


--Results get_taz
r71 = toTId [2,8,10]
r72 = toTId [8,9,10]
r73 = toTId [8,9,10]
r74 = toTId [2,8,9,10]
r75 = toTId [2,8,9,10]
r76 = toTId [8,9,10]
r77 = toTId [2,8,9,10]


--Results get_vsz
r81 = [Sturm,WAC,Austria,WrNeustadt,RBSbg,Groedig,Rapid,Admira,Ried,Altach]
r82 = [Sturm,WAC,Austria,WrNeustadt,RBSbg,Groedig,Rapid,Admira,Ried,Altach]
r83 = [Sturm,WAC,Austria,WrNeustadt,RBSbg,Groedig,Rapid,Admira,Ried,Altach]
r84 = [Sturm,WAC]
r85 = [Sturm,Austria]
r86 = [Sturm,WAC,Austria]
r87 = [Sturm]


--Results get_tmv
r91 = toTId [1,2,3,4,5,6,7,8,9,10]
r92 = toTId [1,2,3,4,5,6,7,8,9,10]
r93 = toTId [1,2,3,4,5,6,7,8,9,10]
r94 = toTId [1,2]
r95 = toTId [1,3]
r96 = toTId [1,2,3]
r97 = toTId [1]




--------------------------------------------------------
----- diese Funktionen brauche ich nur fuers debuggen :)
--------------------------------------------------------
tidToInt :: [TrainerId] -> [Integer]
tidToInt = map ttoInt


ttoInt :: TrainerId -> Integer
ttoInt (TId Z    ) = 0
ttoInt (TId (S(n))) = 1 + ttoInt (TId n)
         
sidToInt :: [SpielerId] -> [Integer]
sidToInt = map stoInt


stoInt :: SpielerId -> Integer
stoInt (SId Z) = 0
stoInt (SId (S(n))) = 1 + stoInt (SId n)