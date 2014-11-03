import Data.List
import Data.Function

type Line = String
type Text = [Line]

maxWordLength :: Text -> Integer
maxWordLength t = fromIntegral(length $ maximumBy (compare `on` length) $ words $ concat t)
--maxWordLength t = words $ concat t



numWordOcc :: Text -> Integer -> Integer
numWordOcc t n | n < 1 = -1


reverseCheck :: Text -> Bool
reverseCheck t = False

formatText :: Text -> Integer -> Text
formatText t n | n < 1 = t

