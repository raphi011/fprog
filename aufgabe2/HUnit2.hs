-- Funktionale Programmierung WS2014
-- Exercise: 2
-- File: HUnit2.hs
-- HUnit test cases

import Aufgabe2
import Test.HUnit

main = runTestTT $ TestList [test1,test2,test3,test4]




test1 = TestLabel "Test: maxWordLength" $ (
        TestList [
        TestCase $ assertEqual "maxWordLength []" (0) (maxWordLength []),
        TestCase $ assertEqual "maxWordLength [\"\"]" (0) (maxWordLength [""]),
        TestCase $ assertEqual "maxWordLength [\"a\"]" (1) (maxWordLength ["a"]),
        TestCase $ assertEqual "maxWordLength [\"aaa\", \" bb\"]" (3) (maxWordLength ["aaa", " bb"]),
        TestCase $ assertEqual "maxWordLength [\"aaa \", \"bb\"]" (3) (maxWordLength ["aaa ", "bb"]),        
        TestCase $ assertEqual "maxWordLength [\"aaa\", \"bb\"]" (5) (maxWordLength ["aaa", "bb"]),
        TestCase $ assertEqual "maxWordLength [\"the quick brown fox\", \" jumps over the lazy dog\"]" (5) (maxWordLength ["the quick brown fox", " jumps over the lazy dog"])
        ]
    )

test2 = TestLabel "Test: numWordOcc" $ (
        TestList [
        TestCase $ assertEqual "numWordOcc [] 0" ((-1)) (numWordOcc [] 0),
        TestCase $ assertEqual "numWordOcc [\"\"] 0" ((-1)) (numWordOcc [""] 0),
        TestCase $ assertEqual "numWordOcc [\"aaaa\"] 0" ((-1)) (numWordOcc ["aaaa"] 0),
        TestCase $ assertEqual "numWordOcc [\"aaaa\", \" bbbb\"] 0" ((-1)) (numWordOcc ["aaaa", " bbbb"] 0),
        TestCase $ assertEqual "numWordOcc [\"aaaa bbb cc d\", \" e ff\"] 3" (1) (numWordOcc ["aaaa bbb cc d", " e ff"] 3),
        TestCase $ assertEqual "numWordOcc [\"aaaa bbb cc d\", \" e ff\"] 2" (2) (numWordOcc ["aaaa bbb cc d", " e ff"] 2),
        TestCase $ assertEqual "numWordOcc [\"aaaa bbb cc d\", \"e ff\"] 2" (3) (numWordOcc ["aaaa bbb cc d", "e ff"] 2),
        TestCase $ assertEqual "numWordOcc [\"Wenn hinter Fliegen Fliegen fliegen,\", \" fliegen Fliegen Fliegen nach\"] 7" (2) (numWordOcc ["Wenn hinter Fliegen Fliegen fliegen,", " fliegen Fliegen Fliegen nach"] 7)
        ]
    )

test3 = TestLabel "Test: reverseCheck" $ (
        TestList [
        TestCase $ assertEqual "reverseCheck []" (False) (reverseCheck []),
        TestCase $ assertEqual "reverseCheck [\"\"]" (True) (reverseCheck [""]),
        TestCase $ assertEqual "reverseCheck [\"\", \"\"]" (True) (reverseCheck ["", ""]),
        TestCase $ assertEqual "reverseCheck [\"a\"]" (True) (reverseCheck ["a"]),
        TestCase $ assertEqual "reverseCheck [\"ab\"]" (False) (reverseCheck ["ab"]),
        TestCase $ assertEqual "reverseCheck [\"abba\"]" (True) (reverseCheck ["abba"]),
        TestCase $ assertEqual "reverseCheck [\"abba\", \"ab\"]" (True) (reverseCheck ["abba", "ab"]),
        TestCase $ assertEqual "reverseCheck [\"ba\", \"ab\"]" (True) (reverseCheck ["ba", "ab"]),
        TestCase $ assertEqual "reverseCheck [\"ab\", \"ab\", \"bvc\", \"ba\"]" (True) (reverseCheck ["ab", "ab", "bvc", "ba"])
        ]
    )

test4 = TestLabel "Test: formatText" $ (
        TestList [
        TestCase $ assertEqual "formatText [] 0" ([]) (formatText [] 0),
        TestCase $ assertEqual "formatText [\"\"] 0" ([""]) (formatText [""] 0),
        TestCase $ assertEqual "formatText [\"aaaa\"] 0" (["aaaa"]) (formatText ["aaaa"] 0),
        TestCase $ assertEqual "formatText [\"aaaa\" , \"bb\"] 0" (["aaaa", "bb"]) (formatText ["aaaa", "bb"] 0),
        TestCase $ assertEqual "formatText [\"aaaa\"] 0" (["aaaa"]) (formatText ["aaaa"] 0),    
        TestCase $ assertEqual "formatText [] 1" ([]) (formatText [] 1),
        TestCase $ assertEqual "formatText [\"aaaa\"] 1" (["a", "a", "a", "a"]) (formatText ["aaaa"] 1),
        TestCase $ assertEqual "formatText [\"aaaa\"] 2" (["aa", "aa"]) (formatText ["aaaa"] 2),
        TestCase $ assertEqual "formatText [\"aa\", \" bbb\"] 2" (["aa", " b", "bb"]) (formatText ["aa", " bbb"] 2),
        TestCase $ assertEqual "formatText [\"aa\", \"bbb\"] 2" (["aa", "bb", "b"]) (formatText ["aa", "bbb"] 2),
        TestCase $ assertEqual "formatText [\"the quick brown fox\", \" jumps over the lazy dog\"] 3" (["the"," qu","ick"," br","own"," fo","x j","ump","s o","ver"," th","e l","azy"," do","g"]) (formatText ["the quick brown fox", " jumps over the lazy dog"] 3)
        ]
    )
