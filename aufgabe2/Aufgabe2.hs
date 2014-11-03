import Data.List
import Data.Ord

type Line = String
type Text = [Line]

words' :: Text -> [String]
words' t = words $ concat t

maxWordLength :: Text -> Integer
maxWordLength t = fromIntegral(length $ maximumBy (comparing length) $  words' t)

countUniques :: (Eq a) => [a] -> Int
countUniques = length . nub

hasLength :: Integer -> String -> Bool
hasLength l s = length s == fromIntegral(l)

numWordOcc :: Text -> Integer -> Integer
numWordOcc t n | n < 1 = -1
numWordOcc t n =  fromIntegral(countUniques $ filter (hasLength n) $ words' t)

reverseCheck :: Text -> Bool
reverseCheck t = False

formatText :: Text -> Integer -> Text
formatText t n | n < 1 = t

