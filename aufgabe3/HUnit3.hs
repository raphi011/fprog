-- Funktionale Programmierung WS2014
-- Exercise: 3
-- File: HUnit3.hs
-- HUnit test cases

import Aufgabe3
import Test.HUnit

main = runTestTT $ TestList [test1,test2,test3]--,test4,test5]





test1 = TestLabel "Test: zeroTest" $ (
        TestList [
        TestCase $ assertEqual "zeroTest []" (False) (zeroTest []),
        TestCase $ assertEqual "zeroTest [0]" (True) (zeroTest [0]),
        TestCase $ assertEqual "zeroTest [1]" (True) (zeroTest [1]),
        TestCase $ assertEqual "zeroTest [0,1]" (True) (zeroTest [0,1]),
        TestCase $ assertEqual "zeroTest [1,0,1]" (True) (zeroTest [1,0,1]),
        TestCase $ assertEqual "zeroTest [0,1,0,1,1]" (True) (zeroTest [0,1,0,1,1]),
        TestCase $ assertEqual "zeroTest [0,0,0,1,1]" (False) (zeroTest [0,0,0,1,1]),
        TestCase $ assertEqual "zeroTest [0,1,0,0,1]" (False) (zeroTest [0,1,0,0,1]),
        TestCase $ assertEqual "zeroTest [0,1,1,0,0]" (False) (zeroTest [0,1,1,0,0]),
        TestCase $ assertEqual "zeroTest [0,0,0,5,1]" (False) (zeroTest [0,0,0,5,1]),
        TestCase $ assertEqual "zeroTest [0,5,0,0,1]" (False) (zeroTest [0,5,0,0,1]),
        TestCase $ assertEqual "zeroTest [0,5,1,0,0]" (False) (zeroTest [0,5,1,0,0]),
        TestCase $ assertEqual "zeroTest [0,1,0,5,1]" (False) (zeroTest [0,1,0,5,1]),
        TestCase $ assertEqual "zeroTest [0,(-1),1,0,0]" (False) (zeroTest [0,(-1),1,0,0]),
        TestCase $ assertEqual "zeroTest [0,1,(-0),1,1]" (True) (zeroTest [0,1,(-0),1,1])
        ]
    )

test2 = TestLabel "Test: numberOf" $ (
        TestList [
        TestCase $ assertEqual "numberOf 0" (0) (numberOf 0),
        TestCase $ assertEqual "numberOf 1" (2) (numberOf 1),
        TestCase $ assertEqual "numberOf 2" (3) (numberOf 2),
        TestCase $ assertEqual "numberOf 3" (5) (numberOf 3),
        TestCase $ assertEqual "numberOf 4" (8) (numberOf 4),
        TestCase $ assertEqual "numberOf 5" (13) (numberOf 5),
        TestCase $ assertEqual "numberOf 10" (144) (numberOf 10),
        TestCase $ assertEqual "numberOf 20" (17711) (numberOf 20),
        TestCase $ assertEqual "numberOf (-0)" (0) (numberOf (-0)),
        TestCase $ assertEqual "numberOf (-1)" (2) (numberOf (-1)),
        TestCase $ assertEqual "numberOf (-2)" (3) (numberOf (-2)),
        TestCase $ assertEqual "numberOf (-3)" (5) (numberOf (-3)),
        TestCase $ assertEqual "numberOf (-4)" (8) (numberOf (-4)),
        TestCase $ assertEqual "numberOf (-5)" (13) (numberOf (-5))
        ]
    )

test3 = TestLabel "Test: minNumOfCoins" $ (
        TestList [
        TestCase $ assertEqual "minNumOfCoins 0" (0) (minNumOfCoins 0),
        TestCase $ assertEqual "minNumOfCoins 1" (1) (minNumOfCoins 1),
        TestCase $ assertEqual "minNumOfCoins 2" (1) (minNumOfCoins 2),
        TestCase $ assertEqual "minNumOfCoins 3" (2) (minNumOfCoins 3),
        TestCase $ assertEqual "minNumOfCoins 4" (2) (minNumOfCoins 4),
        TestCase $ assertEqual "minNumOfCoins 5" (1) (minNumOfCoins 5),
        TestCase $ assertEqual "minNumOfCoins 75" (3) (minNumOfCoins 75),
        TestCase $ assertEqual "minNumOfCoins 388" (8) (minNumOfCoins 388),
        TestCase $ assertEqual "minNumOfCoins (-75)" (3) (minNumOfCoins (-75)),
        TestCase $ assertEqual "minNumOfCoins (-388)" (8) (minNumOfCoins (-388))
        ]
    )

--test4 = TestLabel "Test: numOfSplits" $ (
--        TestList [
        
--        ]
--    )

--test5 = TestLabel "Test: change" $ (
--        TestList [
        
--        ]
--    )
