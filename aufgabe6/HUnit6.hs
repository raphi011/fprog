-- Funktionale Programmierung WS2014
-- Exercise: 6
-- File: HUnit6.hs
-- HUnit test cases

import Aufgabe6
import Test.HUnit


main = runTestTT $ TestList [test1,test2,test3,test4,test5] --,test6]




test1 = TestLabel "Test: mkMatrix" $ (
    TestList [
    TestCase $ assertBool "mkMatrix pm1 (-10)" (checkMatrix rpm1 (mkMatrix pm1 (-10))),
    TestCase $ assertBool "mkMatrix pm2 (-10)" (checkMatrix rpm2 (mkMatrix pm2 (-10))),
    TestCase $ assertBool "mkMatrix pm3 (-10)" (checkMatrix rpm3 (mkMatrix pm3 (-10))),
    TestCase $ assertBool "mkMatrix pm4 (-10)" (checkMatrix rpm4 (mkMatrix pm4 (-10))),
    TestCase $ assertBool "mkMatrix pm5 (-10)" (checkMatrix rpm5 (mkMatrix pm5 (-10)))
    ]
    )

test2 = TestLabel "Test: check Show" $ (
    TestList [
    TestCase $ assertEqual "s1 == show rpm1" (True) (s1 == show rpm1),
    TestCase $ assertEqual "s1 == show rpm2" (False) (s1 == show rpm2),
    TestCase $ assertEqual "s1 == show rpm3" (False) (s1 == show rpm3),
    TestCase $ assertEqual "s1 == show rpm4" (False) (s1 == show rpm4),
    TestCase $ assertEqual "s1 == show rpm5" (False) (s1 == show rpm5),
    TestCase $ assertEqual "s2 == show rpm1" (False) (s2 == show rpm1),
    TestCase $ assertEqual "s2 == show rpm2" (True) (s2 == show rpm2),
    TestCase $ assertEqual "s2 == show rpm3" (False) (s2 == show rpm3),
    TestCase $ assertEqual "s2 == show rpm4" (False) (s2 == show rpm4),
    TestCase $ assertEqual "s2 == show rpm5" (False) (s2 == show rpm5),
    TestCase $ assertEqual "s3 == show rpm1" (False) (s3 == show rpm1),
    TestCase $ assertEqual "s3 == show rpm2" (False) (s3 == show rpm2),
    TestCase $ assertEqual "s3 == show rpm3" (True) (s3 == show rpm3),
    TestCase $ assertEqual "s3 == show rpm4" (False) (s3 == show rpm4),
    TestCase $ assertEqual "s3 == show rpm5" (False) (s3 == show rpm5),
    TestCase $ assertEqual "s4 == show rpm1" (False) (s4 == show rpm1),
    TestCase $ assertEqual "s4 == show rpm2" (False) (s4 == show rpm2),
    TestCase $ assertEqual "s4 == show rpm3" (False) (s4 == show rpm3),
    TestCase $ assertEqual "s4 == show rpm4" (True) (s4 == show rpm4),
    TestCase $ assertEqual "s4 == show rpm5" (False) (s4 == show rpm5),
    TestCase $ assertEqual "s5 == show rpm1" (False) (s5 == show rpm1),
    TestCase $ assertEqual "s5 == show rpm2" (False) (s5 == show rpm2),
    TestCase $ assertEqual "s5 == show rpm3" (False) (s5 == show rpm3),
    TestCase $ assertEqual "s5 == show rpm4" (False) (s5 == show rpm4),
    TestCase $ assertEqual "s5 == show rpm5" (True) (s5 == show rpm5)
    ]
    )

test3 = TestLabel "Test: check Eq" $ (
    TestList [
    TestCase $ assertEqual "rpm1 == rpm1" (True) (rpm1 == rpm1),
    TestCase $ assertEqual "rpm1 == rpm2" (False) (rpm1 == rpm2),
    TestCase $ assertEqual "rpm1 == rpm3" (False) (rpm1 == rpm3),
    TestCase $ assertEqual "rpm1 == rpm4" (False) (rpm1 == rpm4),
    TestCase $ assertEqual "rpm1 == rpm5" (False) (rpm1 == rpm5),
    TestCase $ assertEqual "rpm2 == rpm1" (False) (rpm2 == rpm1),
    TestCase $ assertEqual "rpm2 == rpm2" (True) (rpm2 == rpm2),
    TestCase $ assertEqual "rpm2 == rpm3" (False) (rpm2 == rpm3),
    TestCase $ assertEqual "rpm2 == rpm4" (False) (rpm2 == rpm4),
    TestCase $ assertEqual "rpm2 == rpm5" (False) (rpm2 == rpm5),
    TestCase $ assertEqual "rpm3 == rpm1" (False) (rpm3 == rpm1),
    TestCase $ assertEqual "rpm3 == rpm2" (False) (rpm3 == rpm2),
    TestCase $ assertEqual "rpm3 == rpm3" (True) (rpm3 == rpm3),
    TestCase $ assertEqual "rpm3 == rpm4" (False) (rpm3 == rpm4),
    TestCase $ assertEqual "rpm3 == rpm5" (False) (rpm3 == rpm5),
    TestCase $ assertEqual "rpm4 == rpm1" (False) (rpm4 == rpm1),
    TestCase $ assertEqual "rpm4 == rpm2" (False) (rpm4 == rpm2),
    TestCase $ assertEqual "rpm4 == rpm3" (False) (rpm4 == rpm3),
    TestCase $ assertEqual "rpm4 == rpm4" (True) (rpm4 == rpm4),
    TestCase $ assertEqual "rpm4 == rpm5" (False) (rpm4 == rpm5),
    TestCase $ assertEqual "rpm5 == rpm1" (False) (rpm5 == rpm1),
    TestCase $ assertEqual "rpm5 == rpm2" (False) (rpm5 == rpm2),
    TestCase $ assertEqual "rpm5 == rpm3" (False) (rpm5 == rpm3),
    TestCase $ assertEqual "rpm5 == rpm4" (False) (rpm5 == rpm4),
    TestCase $ assertEqual "rpm5 == rpm5" (True) (rpm5 == rpm5)
    ]
    )

test4 = TestLabel "Test: check OrdMat" $ (
    TestList [
    TestCase $ assertEqual "lsm m1 m1" (False) (lsm m1 m1),
    TestCase $ assertEqual "lsm m1 m2" (True) (lsm m1 m2),
    TestCase $ assertEqual "lsm m1 m3" (False) (lsm m1 m3),
    TestCase $ assertEqual "lsm m2 m1" (False) (lsm m2 m1),
    TestCase $ assertEqual "lsm m2 m2" (False) (lsm m2 m2),
    TestCase $ assertEqual "lsm m2 m3" (False) (lsm m2 m3),
    TestCase $ assertEqual "lsm m3 m1" (False) (lsm m3 m1),
    TestCase $ assertEqual "lsm m3 m2" (False) (lsm m3 m2),
    TestCase $ assertEqual "lsm m3 m3" (False) (lsm m3 m3),
    TestCase $ assertEqual "lem m1 m1" (True) (lem m1 m1),
    TestCase $ assertEqual "lem m1 m2" (True) (lem m1 m2),
    TestCase $ assertEqual "lem m1 m3" (True) (lem m1 m3),
    TestCase $ assertEqual "lem m2 m1" (False) (lem m2 m1),
    TestCase $ assertEqual "lem m2 m2" (True) (lem m2 m2),
    TestCase $ assertEqual "lem m2 m3" (False) (lem m2 m3),
    TestCase $ assertEqual "lem m3 m1" (False) (lem m3 m1),
    TestCase $ assertEqual "lem m3 m2" (True) (lem m3 m2),
    TestCase $ assertEqual "lem m3 m3" (True) (lem m3 m3),
    TestCase $ assertEqual "grm m1 m1" (False) (grm m1 m1),
    TestCase $ assertEqual "grm m1 m2" (False) (grm m1 m2),
    TestCase $ assertEqual "grm m1 m3" (False) (grm m1 m3),
    TestCase $ assertEqual "grm m2 m1" (True) (grm m2 m1),
    TestCase $ assertEqual "grm m2 m2" (False) (grm m2 m2),
    TestCase $ assertEqual "grm m2 m3" (False) (grm m2 m3),
    TestCase $ assertEqual "grm m3 m1" (False) (grm m3 m1),
    TestCase $ assertEqual "grm m3 m2" (False) (grm m3 m2),
    TestCase $ assertEqual "grm m3 m3" (False) (grm m3 m3),
    TestCase $ assertEqual "gem m1 m1" (True) (gem m1 m1),
    TestCase $ assertEqual "gem m1 m2" (False) (gem m1 m2),
    TestCase $ assertEqual "gem m1 m3" (False) (gem m1 m3),
    TestCase $ assertEqual "gem m2 m1" (True) (gem m2 m1),
    TestCase $ assertEqual "gem m2 m2" (True) (gem m2 m2),
    TestCase $ assertEqual "gem m2 m3" (True) (gem m2 m3),
    TestCase $ assertEqual "gem m3 m1" (True) (gem m3 m1),
    TestCase $ assertEqual "gem m3 m2" (False) (gem m3 m2),
    TestCase $ assertEqual "gem m3 m3" (True) (gem m3 m3)
    ]
    )

test5 = TestLabel "Test: check OrderingMat" $ (
    TestList [
    TestCase $ assertEqual "cmpm m1 m1" (EQM) (cmpm m1 m1),
    TestCase $ assertEqual "cmpm m1 m2" (LTM) (cmpm m1 m2),
    TestCase $ assertEqual "cmpm m1 m3" (INC) (cmpm m1 m3),
    TestCase $ assertEqual "cmpm m2 m1" (GTM) (cmpm m2 m1),
    TestCase $ assertEqual "cmpm m2 m2" (EQM) (cmpm m2 m2),
    TestCase $ assertEqual "cmpm m2 m3" (INC) (cmpm m2 m3),
    TestCase $ assertEqual "cmpm m3 m1" (INC) (cmpm m3 m1),
    TestCase $ assertEqual "cmpm m3 m2" (INC) (cmpm m3 m2),
    TestCase $ assertEqual "cmpm m3 m3" (EQM) (cmpm m3 m3)
    ]
)

--test6 = TestLabel "Test: check ArithMat" $ (
--    TestList [
--    TestCase $ assertEqual "addm rpm1 rpm1" (rarpm1rpm1) (addm rpm1 rpm1),
--    TestCase $ assertEqual "addm m1 m3" (ram1m3) (addm m1 m3),
--    TestCase $ assertEqual "addm m1 m1" (ram1m1) (addm m1 m1),
--    TestCase $ assertEqual "addm m1 rpm1" (ram1rpm1) (addm m1 rpm1),
--    TestCase $ assertEqual "multm m1 m1" (rmm1m1) (multm m1 m1),
--    TestCase $ assertEqual "multm m2 m2" (rmm2m2) (multm m2 m2),
--    TestCase $ assertEqual "multm m2 m3" (rmm2m3) (multm m2 m3),
--    TestCase $ assertEqual "multm rpm4 rpm5" (rmrpm4rpm5) (multm rpm4 rpm5),
--    TestCase $ assertEqual "multm rpm1 rpm1" (rmrpm1rpm1) (multm rpm1 rpm1),
--    TestCase $ assertEqual "multm rpm2 rpm3" (rmrpm2rpm3) (multm rpm2 rpm3),
--    TestCase $ assertEqual "multm rpm3 rpm3" (rmrpm3rpm2) (multm rpm3 rpm2)
--    ]
--    )




checkMatrix :: Matrix -> Matrix -> Bool
checkMatrix (M m1) (M m2) = m1 == m2

--toNat :: Integer -> Nat
--toNat n
--    | n < 0 = error "no negative numbers"
--    | n == 0 = Z
--    | otherwise = S (toNat (n-1))

toPosRat :: (Integer,Integer) -> PosRat
toPosRat n = (toNat $ fst n,toNat $ snd n)

toProtoMatrix :: [[(Integer,Integer)]] -> [[Skalar]]
toProtoMatrix pm = [map toPosRat c | c <-pm]

instance Eq Nat where
    Z == Z = True
    (S a) == (S b) = a == b
    _ == _ = False


-- proto matrices
pm1 = toProtoMatrix [ [] ]
pm2 = toProtoMatrix [ [(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1)] ] -- row vector
pm3 = toProtoMatrix [ [(0,1)],[(1,1)],[(2,1)],[(3,1)],[(4,1)],[(5,1)],[(6,1)],[(7,1)],[(8,1)],[(9,1)],[(10,1)] ] -- column vector
pm4 = toProtoMatrix [ [],[(0,1),(1,1),(2,1)],[(3,1)] ]
pm5 = toProtoMatrix [ [(0,1),(1,1),(2,1),(3,1)],[],[],[(4,1)] ]

-- matrices
m1 = M $ toProtoMatrix [ [(1,1),(1,1),(1,1)],[(1,1),(1,1),(1,1)],[(1,1),(1,1),(1,1)] ]
m2 = M $ toProtoMatrix [ [(2,1),(2,1),(2,1)],[(2,1),(2,1),(2,1)],[(2,1),(2,1),(2,1)] ]
m3 = M $ toProtoMatrix [ [(1,1),(2,1),(1,1)],[(1,1),(2,1),(1,1)],[(1,1),(2,1),(1,1)] ]

-- result proto matrices
rpm1 = M $ toProtoMatrix [ [(10,1)] ]
rpm2 = M $ toProtoMatrix [ [(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1)] ] -- row vector
rpm3 = M $ toProtoMatrix [ [(0,1)],[(1,1)],[(2,1)],[(3,1)],[(4,1)],[(5,1)],[(6,1)],[(7,1)],[(8,1)],[(9,1)],[(10,1)] ] -- column vector
rpm4 = M $ toProtoMatrix [ [(10,1),(10,1),(10,1)],[(0,1),(1,1),(2,1)],[(3,1),(10,1),(10,1)] ]
rpm5 = M $ toProtoMatrix [ [(0,1),(1,1),(2,1),(3,1)],[(10,1),(10,1),(10,1),(10,1)],[(10,1),(10,1),(10,1),(10,1)],[(4,1),(10,1),(10,1),(10,1)] ]

--result strings
s1 = "[[([E,Zw],[E])]]"
s2 = "[[([N],[E]),([E],[E]),([Zw],[E]),([D],[E]),([V],[E]),([F],[E]),([Se],[E]),([Si],[E]),([E,N],[E]),([E,E],[E]),([E,Zw],[E])]]"
s3 = "[[([N],[E])],[([E],[E])],[([Zw],[E])],[([D],[E])],[([V],[E])],[([F],[E])],[([Se],[E])],[([Si],[E])],[([E,N],[E])],[([E,E],[E])],[([E,Zw],[E])]]"
s4 = "[[([E,Zw],[E]),([E,Zw],[E]),([E,Zw],[E])],[([N],[E]),([E],[E]),([Zw],[E])],[([D],[E]),([E,Zw],[E]),([E,Zw],[E])]]"
s5 = "[[([N],[E]),([E],[E]),([Zw],[E]),([D],[E])],[([E,Zw],[E]),([E,Zw],[E]),([E,Zw],[E]),([E,Zw],[E])],[([E,Zw],[E]),([E,Zw],[E]),([E,Zw],[E]),([E,Zw],[E])],[([V],[E]),([E,Zw],[E]),([E,Zw],[E]),([E,Zw],[E])]]"

-- result matrices addm
rarpm1rpm1 = M $ toProtoMatrix [ [(20,1)] ]
ram1m3 = M $ toProtoMatrix [ [(2,1),(3,1),(2,1)],[(2,1),(3,1),(2,1)],[(2,1),(3,1),(2,1)] ]
ram1m1 = M $ toProtoMatrix [ [(2,1),(2,1),(2,1)],[(2,1),(2,1),(2,1)],[(2,1),(2,1),(2,1)] ]
ram1rpm1 = M $ [ [(Z,Z)] ]

-- result matrices multm
rmm1m1 = M $ toProtoMatrix [ [(3,1),(3,1),(3,1)],[(3,1),(3,1),(3,1)],[(3,1),(3,1),(3,1)] ]
rmm2m2 = M $ toProtoMatrix [ [(12,1),(12,1),(12,1)],[(12,1),(12,1),(12,1)],[(12,1),(12,1),(12,1)] ]
rmm2m3 = M $ toProtoMatrix [ [(6,1),(12,1),(6,1)],[(6,1),(12,1),(6,1)],[(6,1),(12,1),(6,1)] ]
rmrpm4rpm5 = M $ [ [(Z,Z)] ]
rmrpm1rpm1 = M $ toProtoMatrix [ [(100,1)] ]
rmrpm2rpm3 = M $ toProtoMatrix [ [(385,1)] ]
rmrpm3rpm2 = M $ toProtoMatrix [ [(0,1),(0,1),(0,1),(0,1),(0,1),(0,1),(0,1),(0,1),(0,1),(0,1),(0,1)],
                 [(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1)],
                 [(0,1),(2,1),(4,1),(6,1),(8,1),(10,1),(12,1),(14,1),(16,1),(18,1),(20,1)],
                 [(0,1),(3,1),(6,1),(9,1),(12,1),(15,1),(18,1),(21,1),(24,1),(27,1),(30,1)],
                 [(0,1),(4,1),(8,1),(12,1),(16,1),(20,1),(24,1),(28,1),(32,1),(36,1),(40,1)],
                 [(0,1),(5,1),(10,1),(15,1),(20,1),(25,1),(30,1),(35,1),(40,1),(45,1),(50,1)],
                 [(0,1),(6,1),(12,1),(18,1),(24,1),(30,1),(36,1),(42,1),(48,1),(54,1),(60,1)],
                 [(0,1),(7,1),(14,1),(21,1),(28,1),(35,1),(42,1),(49,1),(56,1),(63,1),(70,1)],
                 [(0,1),(8,1),(16,1),(24,1),(32,1),(40,1),(48,1),(56,1),(64,1),(72,1),(80,1)],
                 [(0,1),(9,1),(18,1),(27,1),(36,1),(45,1),(54,1),(63,1),(72,1),(81,1),(90,1)],
                 [(0,1),(10,1),(20,1),(30,1),(40,1),(50,1),(60,1),(70,1),(80,1),(90,1),(100,1)] ]