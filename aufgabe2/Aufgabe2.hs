type Line = String
type Text = [Line]

maxWordLength :: Text -> Integer
maxWordLength t = 1


numWordOcc :: Text -> Integer -> Integer
numWordOcc t n | n < 1 = -1


reverseCheck :: Text -> Bool
reverseCheck t = False

formatText :: Text -> Integer -> Text
formatText t n | n < 1 = t

