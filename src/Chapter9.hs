module Chapter9 where

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
