module Aufgabe2 where

import Data.List
import Data.Ord

type Line = String
type Text = [Line]

{- words' 
 - takes an array of strings, concats them 
 - and splits them up again
 -}
words' :: Text -> [String]
words' t = words $ concat t

{- maxWordLength
 - gets the length of the longest word from the text
 -}
maxWordLength :: Text -> Integer
maxWordLength t = fromIntegral(length $ maximumBy (comparing length) $ words' t)

{- countUniques 
 - returns a functions the counts distinct values
 - in an array
 -}
countUniques :: (Eq a) => [a] -> Int
countUniques = length . nub

{- hasLength
 - returns a predicate that returns 
 - true if an array has the length n 
 -}
hasLength :: Integer -> String -> Bool
hasLength l s = length s == fromIntegral(l)

{- numWordOcc
 - counts the distinct strings with
 - length n from an array
 -}

numWordOcc :: Text -> Integer -> Integer
numWordOcc t n | n < 1 = -1
numWordOcc t n = fromIntegral(countUniques $ filter (hasLength n) $ words' t)

{- reverseCheck
 - 
 -}
reverseCheck :: Text -> Bool
reverseCheck [] = False
reverseCheck (x:xs) =  x == reverse x || containsReverse xs x || reverseCheck xs

containsReverse :: Text -> String -> Bool
containsReverse [] = False
containsReverse (x:xs) s = reverse x == s || containsReverse xs s

{- formatText
 - takes a text and returns
 - a new text where all lines are
 - max n chars long
 -}
formatText :: Text -> Integer -> Text
formatText t n | n < 1 = t
formatText t n = format (concat t) n 

{- format
 - see formatText
 -}
format :: String -> Integer -> Text
format [] n = []
format t n = [take (fromIntegral n) t] ++ format (drop (fromIntegral n) t) n
