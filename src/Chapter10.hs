module Chapter10 where

import Data.Time

-- Exercises: Database Processing

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr filterDate []
  where
    filterDate (DbDate time) xs = xs ++ [time]
    filterDate _ xs = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filterNumber []
  where
    filterNumber (DbNumber number) xs = xs ++ [number]
    filterNumber _ xs = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr filterRecent (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))
  where
    filterRecent (DbDate time) recent = if time > recent then time else recent
    filterRecent _ recent = recent

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sumNumber 0
  where
    sumNumber (DbNumber n) acc = n + acc
    sumNumber _ acc = acc

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / fromIntegral (length $ filterDbNumber db)

-- Scans Exercises

fibs :: Num a => [a]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Num a => Int -> a
fibsN x = fibs !! x

fibs20 :: Num a => [a]
fibs20 = take 20 fibs

fibsLt100 :: (Num a, Ord a) => [a]
fibsLt100 = takeWhile (< 100) fibs

factorial :: Int -> Int
factorial = (!!) $ scanl (*) 1 [1 ..]

-- 10.10 Chapter Exercises

stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

stopVowelStop :: [(Char, Char, Char)]
stopVowelStop = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

stopVowelStopP :: [(Char, Char, Char)]
stopVowelStopP = [(s1, v, s2) | s1 <- stops, s1 == 'p', v <- vowels, s2 <- stops]

nouns :: [String]
nouns = ["cat", "dog", "elephant"]

verbs :: [String]
verbs = ["eats", "smells", "jumps"]

nounVerbNoun :: [(String, String, String)]
nounVerbNoun = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns]

seekritFunc :: Fractional a => String -> a
seekritFunc x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a acc -> f a || acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\a acc -> a == e || acc) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' = any . (==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a acc -> if f a then a : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldl (\a acc -> if f a acc == GT then a else acc) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldl (\a acc -> if f a acc == LT then a else acc) (head xs) xs
