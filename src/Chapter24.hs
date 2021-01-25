module Chapter24 where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

-- Exercises: Parsing Practice

one :: Parser Char
one = char '1' <* eof

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' <* eof

oneTwelveOrOneHundredTwentyThree :: Parser String
oneTwelveOrOneHundredTwentyThree = (string "123" <|> string "12" <|> string "1") <* eof

stringParser :: String -> Parser String
stringParser = foldr (liftA2 (:) . char) mempty

-- Exercise: Unit of Success

yourFuncHere :: Parser Integer
yourFuncHere = integer <* eof

-- Exercise: Try Try

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  return (numerator % denominator)

type DoubleOrFraction = Either Double Rational

parseDof :: Parser DoubleOrFraction
parseDof = Left <$> try double <|> Right <$> try parseFraction
