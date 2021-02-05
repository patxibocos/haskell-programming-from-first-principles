module Chapter24 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Ratio ((%))
import Data.Time (TimeOfDay (TimeOfDay, todHour, todMin))
import Text.Printf
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

-- 24.11 Chapter Exercises

data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Ord, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show)

parseSemVer :: Parser SemVer
parseSemVer = do
  (major, minor, patch) <- parseVersion
  release <- option [] parseRelease
  metadata <- option [] parseMetadata
  return $ SemVer major minor patch release metadata

parseVersion :: Parser (Major, Minor, Patch)
parseVersion = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  return (major, minor, patch)

parseRelease :: Parser [NumberOrString]
parseRelease = char '-' *> parseNumberOrString `sepBy` char '.'

parseMetadata :: Parser [NumberOrString]
parseMetadata = char '+' *> parseNumberOrString `sepBy` char '.'

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = NOSI <$> try (integer <* notFollowedBy anyChar) <|> NOSS <$> some alphaNum

instance Eq SemVer where
  (SemVer major1 minor1 patch1 release1 _) == (SemVer major2 minor2 patch2 release2 _) =
    major1 == major2 && minor1 == minor2 && patch1 == patch2 && release1 == release2

instance Ord SemVer where
  (SemVer major1 minor1 patch1 release1 _) `compare` (SemVer major2 minor2 patch2 release2 _) =
    major1 `compare` major2
      <> minor1 `compare` minor2
      <> patch1 `compare` patch2
      <> release1 `compareRelease` release2
    where
      [] `compareRelease` [] = EQ
      [] `compareRelease` _ = GT
      _ `compareRelease` [] = LT
      r1 `compareRelease` r2 = r1 `compare` r2

parseDigit :: Parser Char
parseDigit = oneOf ['0' .. '9']

base10Integer :: Parser Integer
base10Integer = toInteger . foldl (\a b -> a * 10 + digitToInt b) 0 <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = char '-' *> (negate <$> base10Integer) <|> base10Integer

-- aka area code
type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber
  = PhoneNumber
      NumberingPlanArea
      Exchange
      LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  numberingPlanArea <- parseNumberingPlanArea
  _ <- skipSeparator
  exchange <- parseExchange
  _ <- skipSeparator
  lineNumber <- parseLineNumber
  return $ PhoneNumber numberingPlanArea exchange lineNumber
  where
    skipSeparator = skipOptional $ oneOf " -"

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea =
  read
    <$> ( between (char '(') (char ')') (digits 3) <* space
            <|> string "1-" *> digits 3
            <|> digits 3
        )

parseExchange :: Parser Exchange
parseExchange = read <$> digits 3

parseLineNumber :: Parser LineNumber
parseLineNumber = read <$> digits 4

digits :: Int -> Parser String
digits n = replicateM n digit

newtype Date = Date (Int, Int, Int)

instance Show Date where
  show (Date (year, month, day)) = printf "%04d" year ++ "-" ++ printf "%02d" month ++ "-" ++ printf "%02d" day

data Activity = Activity TimeOfDay ActivityName

instance Show Activity where
  show (Activity time activity) =
    printf "%02d" (todHour time)
      ++ ":"
      ++ printf "%02d" (todMin time)
      ++ " "
      ++ activity

data Day = Day Date [Activity]

instance Show Day where
  show (Day date activities) =
    "# "
      ++ show date
      ++ "\n"
      ++ intercalate "\n" (fmap show activities)

type ActivityName = String

newtype Log = Log [Day]

instance Show Log where
  show (Log days) = intercalate "\n\n" (fmap show days)

parseLog :: Parser Log
parseLog = do
  _ <- skipMany (void newline <|> parseComment)
  days <- some parseDay
  _ <- skipMany newline
  return $ Log days

parseDay :: Parser Day
parseDay = liftA2 Day parseDate (some parseActivity)

parseDate :: Parser Date
parseDate = do
  _ <- string "# "
  year <- read <$> digits 4
  _ <- char '-'
  month <- read <$> digits 2
  _ <- char '-'
  day <- read <$> digits 2
  _ <- skipOptional parseComment
  _ <- newline
  return $ Date (year, month, day)

parseActivity :: Parser Activity
parseActivity = do
  hour <- read <$> digits 2
  _ <- char ':'
  minute <- read <$> digits 2
  _ <- space
  activity <- try (manyTill (noneOf "\n") (try parseComment)) <|> some (noneOf "\n")
  _ <- skipMany newline
  return $ Activity (TimeOfDay hour minute 0) activity

parseComment :: Parser ()
parseComment = skipOptional (char ' ') >> string "-- " >> skipMany (noneOf "\n")
