module Chapter24 where

import Control.Applicative
import Text.Trifecta

-- Exercises: Parsing Practice

one :: Parser Char
one = char '1' <* eof

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' <* eof

oneTwelveOrOneHundredTwentyThree :: Parser String
oneTwelveOrOneHundredTwentyThree = (string "123" <|> string "12" <|> string "1") <* eof

stringParser :: String -> Parser [Char]
stringParser [] = mempty
stringParser (x : xs) = do
  a <- char x
  as <- stringParser xs
  return (a : as)
