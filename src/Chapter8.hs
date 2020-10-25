module Chapter8 where

import Data.List (intersperse)

sums :: (Eq a, Num a) => a -> a
sums 1 = 1
sums n = n + sums (n -1)

recursiveSum :: (Integral a) => a -> a -> a
recursiveSum x y = go y 0
  where
    go times acc
      | times == 0 = acc
      | otherwise = go (times - 1) (acc + x)

data DividedResult = Result Integer | DividedByZero

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy x y = Result $ go x 0
  where
    go rest acc
      | rest < y = acc
      | otherwise = go (rest - y) (acc + 1)

mc91 :: Integral a => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 $ mc91 $ n + 11

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eigth"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
