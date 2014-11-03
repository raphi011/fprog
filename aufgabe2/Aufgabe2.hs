import Data.List
import Data.Function

type Line = String
type Text = [Line]

words' :: Text -> Line
words' t = words $ concat t

maxWordLength :: Text -> Integer
maxWordLength t = fromIntegral(length $ maximumBy (compare `on` length) $  words' t)

numWordOcc :: Text -> Integer -> Integer
numWordOcc t n | n < 1 = -1
numWordOcc t n =  length . nub $ filter (\x (length x == n)) $ words' t


reverseCheck :: Text -> Bool
reverseCheck t = False

formatText :: Text -> Integer -> Text
formatText t n | n < 1 = t

