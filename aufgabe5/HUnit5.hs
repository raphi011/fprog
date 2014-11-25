-- Funktionale Programmierung WS2014
-- Exercise: 5
-- File: HUnit5.hs
-- HUnit test cases

import Aufgabe5
import Test.HUnit

main = runTestTT $ TestList [test1,test2,test3,test4,test5,test6,test7,test8]




test1 = TestLabel "Test: tcm" $ (
    TestList [
    TestCase $ assertEqual "tcm (>=1) (+100) t1" (t2st $ tcm1t1) ((t2st $ tcm (>=1) (+100) t1)),
    TestCase $ assertEqual "tcm (>=10) (+100) t1" (t2st $ tcm2t1) ((t2st $ tcm (>=10) (+100) t1)),
    TestCase $ assertEqual "tcm (==2) (+100) t2" (t2st $ tcm3t2) ((t2st $ tcm (==2) (+100) t2)),
    TestCase $ assertEqual "tcm (>=1) (+100) t3" (t2st $ tcm4t3) ((t2st $ tcm (>=1) (+100) t3)),
    TestCase $ assertEqual "tcm (even) (+100) t3" (t2st $ tcm5t3) ((t2st $ tcm (even) (+100) t3)),
    TestCase $ assertEqual "tcm (>=3) (+100) t3" (t2st $ tcm6t3) ((t2st $ tcm (>=3) (+100) t3)),    
    TestCase $ assertEqual "tcm (>=10) (+100) t3" (t2st $ tcm7t3) ((t2st $ tcm (>=10) (+100) t3)),
    TestCase $ assertEqual "tcm (odd) (+100) t51" (t2st $ tcm9t51) ((t2st $ tcm (odd) (+100) t51))
    ]
    )

test2 = TestLabel "Test: tzp" $ (
    TestList [
    TestCase $ assertEqual "tzp (-) t1 t1" (t2st $ tzp1t1t1) ((t2st $ tzp (-) t1 t1)),
    TestCase $ assertEqual "tzp (-) t1 t6" (t2st $ tzp2t1t6) ((t2st $ tzp (-) t1 t6)),
    TestCase $ assertEqual "tzp (-) t7 t8" (t2st $ tzp3t7t8) ((t2st $ tzp (-) t7 t8)),
    TestCase $ assertEqual "tzp (-) t8 t7" (t2st $ tzp4t8t7) ((t2st $ tzp (-) t8 t7)),
    TestCase $ assertEqual "tzp (-) t8 t8" (t2st $ tzp5t8t8) ((t2st $ tzp (-) t8 t8)),
    TestCase $ assertEqual "tzp (-) t7 t9" (t2st $ tzp6t7t9) ((t2st $ tzp (-) t7 t9))
    ]
    )

test3 = TestLabel "Test: tmax" $ (
    TestList [
    TestCase $ assertEqual "(tmax 0 t1) 0" (1) ((tmax 0 t1) 0),
    TestCase $ assertEqual "(tmax 0 t2) 0" (3) ((tmax 0 t2) 0),
    TestCase $ assertEqual "(tmax 0 t3) 0" (42) ((tmax 0 t3) 0),
    TestCase $ assertEqual "(tmax 0 t4) 0" (42) ((tmax 0 t4) 0),
    TestCase $ assertEqual "(tmax 0 t5) 0" (42) ((tmax 0 t5) 0),
    TestCase $ assertEqual "(tmax 0 t51) 0" (51) ((tmax 0 t51) 0)
    ]
    )

test4 = TestLabel "Test: t2st" $ (
    TestList [
    TestCase $ assertEqual "t2st t1" (s1) (t2st t1),
    TestCase $ assertEqual "t2st t2" (s2) (t2st t2),
    TestCase $ assertEqual "t2st t3" (s3) (t2st t3),
    TestCase $ assertEqual "t2st t4" (s4) (t2st t4),
    TestCase $ assertEqual "t2st t5" (s5) (t2st t5),
    TestCase $ assertEqual "t2st t51" (s51) (t2st t51)
    ]
    )

test5 = TestLabel "Test: tsum" $ (
    TestList [
    TestCase $ assertEqual "tsum s1" (1) (tsum s1),
    TestCase $ assertEqual "tsum s2" (6) (tsum s2),
    TestCase $ assertEqual "tsum s3" (21) (tsum s3),
    TestCase $ assertEqual "tsum s4" (28) (tsum s4),
    TestCase $ assertEqual "tsum s5" (78) (tsum s5),
    TestCase $ assertEqual "tsum s51" (1326) (tsum s51)
    ]
    )

test6 = TestLabel "Test: tdepth" $ (
    TestList [
    TestCase $ assertEqual "tdepth s1" (1) (tdepth s1),
    TestCase $ assertEqual "tdepth s2" (2) (tdepth s2),
    TestCase $ assertEqual "tdepth s3" (3) (tdepth s3),
    TestCase $ assertEqual "tdepth s4" (4) (tdepth s4),
    TestCase $ assertEqual "tdepth s5" (6) (tdepth s5),
    TestCase $ assertEqual "tdepth s51" (50) (tdepth s51)
    ]
    )

test7 = TestLabel "Test: check functions of tcm" $ (
    TestList [
    TestCase $ assertEqual "checkTrees tcm1t1 (tcm (>=1) (+100) t1)" (True) (checkTrees tcm1t1 (tcm (>=1) (+100) t1)),
    TestCase $ assertEqual "checkTrees tcm2t1 (tcm (>=10) (+100) t1)" (True) (checkTrees tcm2t1 (tcm (>=10) (+100) t1)),
    TestCase $ assertEqual "checkTrees tcm3t2 (tcm (==2) (+100) t2)" (True) (checkTrees tcm3t2 (tcm (==2) (+100) t2)),
    TestCase $ assertEqual "checkTrees tcm4t3 (tcm (>=1) (+100) t3)" (True) (checkTrees tcm4t3 (tcm (>=1) (+100) t3)),
    TestCase $ assertEqual "checkTrees tcm5t3 (tcm (even) (+100) t3)" (True) (checkTrees tcm5t3 (tcm (even) (+100) t3)),
    TestCase $ assertEqual "checkTrees tcm6t3 (tcm (>=3) (+100) t3)" (True) (checkTrees tcm6t3 (tcm (>=3) (+100) t3)),    
    TestCase $ assertEqual "checkTrees tcm7t3 (tcm (>=10) (+100) t3)" (True) (checkTrees tcm7t3 (tcm (>=10) (+100) t3)),
    TestCase $ assertEqual "checkTrees tcm9t51 (tcm (odd) (+100) t51)" (True) (checkTrees tcm9t51 (tcm (odd) (+100) t51))
    ]
    )

test8 = TestLabel "Test: check functions of tzp" $ (
    TestList [
    TestCase $ assertEqual "checkTrees tzp1t1t1 (tzp (-) t1 t1)" (True) (checkTrees tzp1t1t1 (tzp (-) t1 t1)),
    TestCase $ assertEqual "checkTrees tzp2t1t6 (tzp (-) t1 t6)" (True) (checkTrees tzp2t1t6 (tzp (-) t1 t6)),
    TestCase $ assertEqual "checkTrees tzp3t7t8 (tzp (-) t7 t8)" (True) (checkTrees tzp3t7t8 (tzp (-) t7 t8)),
    TestCase $ assertEqual "checkTrees tzp4t8t7 (tzp (-) t8 t7)" (True) (checkTrees tzp4t8t7 (tzp (-) t8 t7)),
    TestCase $ assertEqual "checkTrees tzp5t8t8 (tzp (-) t8 t8)" (True) (checkTrees tzp5t8t8 (tzp (-) t8 t8)),
    TestCase $ assertEqual "checkTrees tzp6t7t9 (tzp (-) t7 t9)" (True) (checkTrees tzp6t7t9 (tzp (-) t7 t9))
    ]
    )




checkTrees :: Tree -> Tree -> Bool
checkTrees calculated expected = (t2st $ executeFunction calculated) == (t2st $ executeFunction expected)

executeFunction :: Tree -> Tree
executeFunction(Node f l t) = (Node f (f l) (map (executeFunction) t))


-- trees
t1 = Node (+1) 1 []
t2 = Node (+1) 1 [Node (+2) 2 [], Node (+3) 3 []]
t3 = Node (+42) 1 [Node (+2) 2 [Node (+4) 4 [], Node (+5) 5 [], Node (+6) 6 []], Node (+3) 3 []]
t4 = Node (+1) 1 [Node (+2) 2 [Node (+4) 4 [], Node (+5) 5 [Node (+7) 7 []], Node (+42) 6 []], Node (+3) 3 []]
t5 = Node (+1) 1 [Node (+2) 2 [Node (+4) 4 [], Node (+42) 5 [Node (+7) 7 [], Node (+8) 8 [Node (+10) 10 [], Node (+11) 11 [Node (+12) 12 []]], Node (+9) 9 []], Node (+42) 6 []], Node (+3) 3 []]
t51 = Node (+1) 1 [Node (+2) 2 [Node (+3) 3 [Node (+4) 4 [Node (+5) 5 [Node (+6) 6 [Node (+7) 7 [Node (+8) 8 [Node (+9) 9 [Node (+10) 10 [Node (+11) 11 [Node (+12) 12 [Node (+13) 13 [Node (+14) 14 [Node (+15) 15 [Node (+16) 16 [Node (+17) 17 [Node (+18) 18 [Node (+19) 19 [Node (+20) 20 [Node (+21) 21 [Node (+22) 22 [Node (+23) 23 [Node (+24) 24 [Node (+25) 25 [Node (+26) 26 [Node (+27) 27 [Node (+28) 28 [Node (+29) 29 [Node (+30) 30 [Node (+31) 31 [Node (+32) 32 [Node (+33) 33 [Node (+34) 34 [Node (+35) 35 [Node (+36) 36 [Node (+37) 37 [Node (+38) 38 [Node (+39) 39 [Node (+40) 40 [Node (+41) 41 [Node (+42) 42 [Node (+43) 43 [Node (+44) 44 [Node (+45) 45 [Node (+46) 46 [Node (+47) 47 [Node (+48) 48 [Node (+49) 49 [Node (+50) 50 [], Node (+51) 51 []]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
t6 = Node (+1) 1 [Node (+2) 2 []]
t7 = Node (+1) 1 [Node (+2) 2 [Node (+3) 3 [], Node (+4) 4 []], Node (+5) 5 []]
t8 = Node (+1) 1 [Node (+2) 2 [], Node (+3) 3 [Node (+4) 4 [], Node (+5) 5 []]]
t9 = Node (+5) 5 [Node (+4) 4 [Node (+3) 3 [], Node (+2) 2 []], Node (+1) 1 []]

-- tcm result trees
tcm1t1 = Node (+1) 101 []
tcm2t1 = Node (+1) 2 []
tcm3t2 = Node (+1) 2 [Node (+2) 102 [], Node (+3) 6 []]
tcm4t3 = Node (+42) 101 [Node (+2) 102 [Node (+4) 104 [], Node (+5) 105 [], Node (+6) 106 []], Node (+3) 103 []]
tcm5t3 = Node (+42) 43 [Node (+2) 102 [Node (+4) 104 [], Node (+5) 10 [], Node (+6) 106 []], Node (+3) 6 []]
tcm6t3 = Node (+42) 43 [Node (+2) 4 [Node (+4) 104 [], Node (+5) 105 [], Node (+6) 106 []], Node (+3) 103 []]
tcm7t3 = Node (+42) 43 [Node (+2) 4 [Node (+4) 8 [], Node (+5) 10 [], Node (+6) 12 []], Node (+3) 6 []]
tcm8t5 = Node (+1) 1 [Node (+2) 2 [Node (+4) 4 [], Node (+42) 5 [Node (+7) 7 [], Node (+8) 8 [Node (+10) 10 [], Node (+11) 11 [Node (+12) 12 []]], Node (+9) 9 []], Node (+42) 6 []], Node (+3) 3 []]
tcm9t51 = Node (+1) 101 [Node (+2) 4 [Node (+3) 103 [Node (+4) 8 [Node (+5) 105 [Node (+6) 12 [Node (+7) 107 [Node (+8) 16 [Node (+9) 109 [Node (+10) 20 [Node (+11) 111 [Node (+12) 24 [Node (+13) 113 [Node (+14) 28 [Node (+15) 115 [Node (+16) 32 [Node (+17) 117 [Node (+18) 36 [Node (+19) 119 [Node (+20) 40 [Node (+21) 121 [Node (+22) 44 [Node (+23) 123 [Node (+24) 48 [Node (+25) 125 [Node (+26) 52 [Node (+27) 127 [Node (+28) 56 [Node (+29) 129 [Node (+30) 60 [Node (+31) 131 [Node (+32) 64 [Node (+33) 133 [Node (+34) 68 [Node (+35) 135 [Node (+36) 72 [Node (+37) 137 [Node (+38) 76 [Node (+39) 139 [Node (+40) 80 [Node (+41) 141 [Node (+42) 84 [Node (+43) 143 [Node (+44) 88 [Node (+45) 145 [Node (+46) 92 [Node (+47) 147 [Node (+48) 96 [Node (+49) 149 [Node (+50) 100 [], Node (+51) 151 []]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

-- tzp result trees
tzp1t1t1 = Node ((-)1) 3 []
tzp2t1t6 = Node ((-)1) 3 []
tzp3t7t8 = Node ((-)1) 3 [Node ((-)2) 6 [], Node (+8) 2 []]
tzp4t8t7 = Node ((-)1) 3 [Node ((-)2) 6 [], Node (+8) (-2) []]
tzp5t8t8 = Node ((-)1) 3 [Node ((-)2) 6 [], Node ((-)3) 9 [Node ((-)4) 12 [], Node ((-)5) 15 []]]
tzp6t7t9 = Node (+6) (-4) [Node (+6) (-2) [Node ((-)3) 9 [], Node (+6) 2 []], Node (+6) 4 []]

-- strees
s1 = SNode 1 []
s2 = SNode 1 [SNode 2 [], SNode 3 []]
s3 = SNode 1 [SNode 2 [SNode 4 [], SNode 5 [], SNode 6 []], SNode 3 []]
s4 = SNode 1 [SNode 2 [SNode 4 [], SNode 5 [SNode 7 []], SNode 6 []], SNode 3 []]
s5 = SNode 1 [SNode 2 [SNode 4 [], SNode 5 [SNode 7 [], SNode 8 [SNode 10 [], SNode 11 [SNode 12 []]], SNode 9 []], SNode 6 []], SNode 3 []]
s51= SNode 1 [SNode 2 [SNode 3 [SNode 4 [SNode 5 [SNode 6 [SNode 7 [SNode 8 [SNode 9 [SNode 10 [SNode 11 [SNode 12 [SNode 13 [SNode 14 [SNode 15 [SNode 16 [SNode 17 [SNode 18 [SNode 19 [SNode 20 [SNode 21 [SNode 22 [SNode 23 [SNode 24 [SNode 25 [SNode 26 [SNode 27 [SNode 28 [SNode 29 [SNode 30 [SNode 31 [SNode 32 [SNode 33 [SNode 34 [SNode 35 [SNode 36 [SNode 37 [SNode 38 [SNode 39 [SNode 40 [SNode 41 [SNode 42 [SNode 43 [SNode 44 [SNode 45 [SNode 46 [SNode 47 [SNode 48 [SNode 49 [SNode 50 [], SNode 51 []]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]