module Chapter9 where

import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

eft :: (Ord a, Enum a) => a -> a -> [a]
eft f t
  | f > t = []
  | f == t = [f]
  | otherwise = f : eft (succ f) t

myWords :: String -> [String]
myWords "" = []
myWords (' ' : xs) = myWords xs
myWords phrase = takeWhile (/= ' ') phrase : myWords (dropWhile (/= ' ') phrase)

myLines :: String -> [String]
myLines "" = []
myLines ('\n' : xs) = myLines xs
myLines phrase = takeWhile (/= '\n') phrase : myLines (dropWhile (/= '\n') phrase)

mySplit :: String -> Char -> [String]
mySplit "" _ = []
mySplit (x : xs) a
  | x == a = mySplit xs a
  | otherwise = takeWhile (/= a) (x : xs) : mySplit (dropWhile (/= a) (x : xs)) a

mySqr :: (Num a, Enum a) => [a]
mySqr = [x ^ 2 | x <- [1 .. 5]]

myCube :: (Num a, Enum a) => [a]
myCube = [y ^ 3 | y <- [1 .. 5]]

mySqrCube :: (Num a, Enum a) => [(a, a)]
mySqrCube = [(x, y) | x <- mySqr, y <- myCube]

mySqrCubeLt50 :: (Num a, Enum a, Ord a) => [(a, a)]
mySqrCubeLt50 = [(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50]

lengthMySqrCubeLt50 :: [(a, a)] -> Int
lengthMySqrCubeLt50 = length

-- 9.10 Filtering lists of values

filterMultipleOf3 :: Integral a => [a]
filterMultipleOf3 = filter (\x -> x `rem` 3 == 0) [1 .. 30]

filterMultipleOf3Count :: Int
filterMultipleOf3Count = length . filter (\x -> x `rem` 3 == 0) $ [1 .. 30]

myFilter :: String -> [String]
myFilter = filter (`notElem` ["the", "a", "an"]) . words

-- 9.11 Zipping lists

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

zipFromZipWith :: [a] -> [b] -> [(a, b)]
zipFromZipWith = myZipWith (,)

-- 9.12 Chapter Exercises

filterUppers :: String -> String
filterUppers = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

upperString :: String -> String
upperString [] = []
upperString (x : xs) = toUpper x : upperString xs

headCapitalized :: String -> Char
headCapitalized = toUpper . head

shift :: Int -> Char -> Char
shift s c
  | c `elem` ['a' .. 'z'] = fixedShift 'a'
  | c `elem` ['A' .. 'Z'] = fixedShift 'A'
  | otherwise = error "character must be a letter"
  where
    shiftedOrd = ord c + s
    fixedShift start = chr $ ord start + ((shiftedOrd - ord start) `mod` 26)

caesar :: String -> String
caesar = map $ shift 3

unCaesar :: String -> String
unCaesar = map $ shift (-3)

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = if x then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = if (f x) then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x : xs) = if e == x then True else myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e xs = any (== e) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "list must not be empty"
myMaximumBy _ [x] = x
myMaximumBy f (x : y : xs) = if f x y == GT then myMaximumBy f (x : xs) else myMaximumBy f (y : xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "list must not be empty"
myMinimumBy _ [x] = x
myMinimumBy f (x : y : xs) = if f x y == LT then myMinimumBy f (x : xs) else myMinimumBy f (y : xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare