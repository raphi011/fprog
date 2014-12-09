#! runghc

-- Module ----------------------------------------------------------------------

module Main where

-- Imports ---------------------------------------------------------------------

import Aufgabe7
import Test.HUnit

-- Data ------------------------------------------------------------------------

zero  = Z
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five

playerIronMan  = SId zero
playerSeaman   = SId one
playerHulk     = SId two
playerTesla    = SId three
playerAda      = SId four
playerHirscher = SId five
playerGandhi   = SId six

trainerMickeyMouse      = TId zero
trainerKoller           = TId one
trainerPiMan            = TId two
trainerHeMan            = TId three
trainerImaginaryNumbers = TId four

playersOne = Kd playerMappingOne
playersTwo = Kd playerMappingTwo
trainersOne = Tr trainerMappingOne
trainersTwo = Tr trainerMappingTwo

seasonOne = (pointMappingOne, playersOne, trainersOne)
seasonTwo = (pointMappingTwo, playersTwo, trainersTwo)

history = [seasonOne, seasonTwo]

{-

Order: Sturm, WAC, Austria, WrNeustadt, RBSbg, Groedig, Rapid, Admira, Ried,
       Altach

Season 1:

Place   Points  Team        Trainer         Players
--------------------------------------------------------------------------------
    1       5   Groedig     He-Man          Ada, Seaman
    2       5   Rapid       He-Man          Ada, Seaman
    3       5   Admira      He-Man          Ada, Seaman
    4       5   Ried        He-Man          Ada, Seaman
    5       5   Altach      He-Man          Ada, Seaman
    6       4   RBSbg       He-Man          Ada, Seaman
    7       3   Austria     PI Man          Hulk, Tesla
    8       3   WrNeustadt  He-Man          Ada, Seaman
    9       2   WAC         Koller          Hulk, Seaman
   10       1   Sturm       Mickey Mouse    Iron Man, Seaman

Season 2:

Place   Points  Team        Trainer             Players
--------------------------------------------------------------------------------
    1       5   Ried        He-Man              Ada, Hulk, Seaman, Tesla
    2       4   Sturm       Mickey Mouse        Ada, Hirscher
    3       4   WAC         Imaginary Numbers   Gandhi
    4       4   Austria     Koller              Tesla
    5       4   WrNeustadt  He-Man              Ada, Hulk, Seaman, Tesla
    6       4   RBSbg       He-Man              Ada, Hulk, Seaman, Tesla
    7       4   Rapid       He-Man              Ada, Hulk, Seaman, Tesla
    8       4   Altach      He-Man              Ada, Hulk, Seaman, Tesla
    9       3   Admira      He-Man              Ada, Hulk, Seaman, Tesla
   10       1   Groedig     He-Man              Ada, Hulk, Seaman, Tesla

-}

pointMappingOne :: Verein -> Nat
pointMappingOne Sturm = one
pointMappingOne WAC = two
pointMappingOne Austria = three
pointMappingOne WrNeustadt = three
pointMappingOne RBSbg = four
pointMappingOne _ = five

trainerMappingOne :: Verein -> TrainerId
trainerMappingOne Sturm = trainerMickeyMouse
trainerMappingOne WAC = trainerKoller
trainerMappingOne Austria = trainerPiMan
trainerMappingOne _ = trainerHeMan

playerMappingOne :: Verein -> [SpielerId]
playerMappingOne Sturm = [playerIronMan, playerSeaman]
playerMappingOne WAC = [playerHulk, playerSeaman]
playerMappingOne Austria = [playerHulk, playerTesla]
playerMappingOne _ = [playerAda, playerSeaman]

pointMappingTwo :: Verein -> Nat
pointMappingTwo Groedig = one
pointMappingTwo Rapid = four
pointMappingTwo Admira = three
pointMappingTwo Ried = five
pointMappingTwo Altach = four
pointMappingTwo _ = four

trainerMappingTwo :: Verein -> TrainerId
trainerMappingTwo Sturm = trainerMickeyMouse
trainerMappingTwo WAC = trainerImaginaryNumbers
trainerMappingTwo Austria = trainerKoller
trainerMappingTwo _ = trainerHeMan

playerMappingTwo :: Verein -> [SpielerId]
playerMappingTwo Sturm = [playerHirscher, playerAda]
playerMappingTwo WAC = [playerGandhi]
playerMappingTwo Austria = [playerTesla]
playerMappingTwo _ = [playerSeaman, playerHulk, playerAda, playerTesla]

-- (1) -------------------------------------------------------------------------

test_get_spm = TestLabel "Test: get_spm" $ (
    TestList [
        TestCase $ assertEqual "get_spm history == [playerSeaman, playerAda]"
            ([playerSeaman, playerAda])
            (get_spm history)
        ])

-- (2) -------------------------------------------------------------------------

test_get_mdhm = TestLabel "Test: get_mdhm" $ (
    TestList [
        TestCase $ assertEqual "get_mdhm history == [Rapid]"
            ([Rapid])
            (get_mdhm history)
        ])

-- (3) -------------------------------------------------------------------------

test_get_mdhi = TestLabel "Test: get_mdhi" $ (
    TestList [
        TestCase $ assertEqual "get_mdhi history == [Ried]"
            ([Ried])
            (get_mdhi history)
        ])

-- (4) -------------------------------------------------------------------------

test_get_pv = TestLabel "Test: get_pv" $ (
    TestList [
        TestCase $ assertEqual "get_pv history == [playerHirscher]"
            ([playerHirscher])
            (get_pv history)
        ])

-- (5) -------------------------------------------------------------------------

test_get_ugr = TestLabel "Test: get_ugr" $ (
    TestList [
        TestCase $ assertEqual "get_ugr history == [playerSeaman]"
            ([playerSeaman])
            (get_ugr history)
        ])

-- (6) -------------------------------------------------------------------------

test_get_tsp = TestLabel "Test: get_tsp" $ (
    TestList [
        TestCase $ assertEqual "get_tsp history == [trainerHeMan]"
            ([trainerHeMan])
            (get_tsp history)
        ])

-- (7) -------------------------------------------------------------------------

test_get_taz = TestLabel "Test: get_taz" $ (
    TestList [
        TestCase $ assertEqual "get_taz history == [trainerHeMan]"
            ([trainerHeMan])
            (get_taz history)
        ])

-- (8) -------------------------------------------------------------------------

test_get_vsz = TestLabel "Test: get_vsz" $ (
    TestList [
        TestCase $ assertEqual "get_vsz history == [WAC, Austria]"
            ([WAC, Austria])
            (get_vsz history)
        ])

-- (9) -------------------------------------------------------------------------

test_get_tmv = TestLabel "Test: get_tmv" $ (
    TestList [
        TestCase $ assertEqual "get_tmv history == [trainerHeMan]"
            ([trainerHeMan])
            (get_tmv history)
        ])

-- (10) ------------------------------------------------------------------------

test_get_tps = TestLabel "Test: get_tps" $ (
    TestList [
        TestCase $ assertEqual
            "get_tps history == [trainerMickeyMouse, trainerHeMan]"
            ([trainerMickeyMouse, trainerHeMan])
            (get_tps history)
        ])

-- Test-List -------------------------------------------------------------------

everything = TestList [
    test_get_spm,
    test_get_mdhm,
    test_get_mdhi,
    test_get_pv,
    test_get_ugr,
    test_get_tsp,
    test_get_taz,
    test_get_vsz,
    test_get_tmv,
    test_get_tps]

-- Main ------------------------------------------------------------------------

main = do
		test <- (runTestTT everything)
		print (test)
