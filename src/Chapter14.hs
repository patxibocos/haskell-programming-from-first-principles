{-# LANGUAGE TemplateHaskell #-}

module Chapter14 where

import Chapter11 (capitalizeWord)
import Chapter8 (digitToWord, digits, wordNumber)
import Data.List
import Test.Hspec
import Test.QuickCheck

-- 14.7 Chapter Exercises

-- Validating numbers into words

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

-- Using QuickCheck

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (* 2) . half

prop_halfIdentity :: Property
prop_halfIdentity = forAll (arbitrary :: Gen Double) (\x -> halfIdentity x == x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: Property
prop_listOrdered = forAll (arbitrary :: Gen [Int]) (listOrdered . sort)

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusAssociative :: Property
prop_plusAssociative = forAll (arbitrary :: Gen (Int, Int, Int)) (\(x, y, z) -> plusAssociative x y z)

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

prop_plusCommutative :: Property
prop_plusCommutative = forAll (arbitrary :: Gen (Int, Int)) (uncurry plusCommutative)

multAssociative :: Int -> Int -> Int -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

prop_multAssociative :: Property
prop_multAssociative = forAll (arbitrary :: Gen (Int, Int, Int)) (\(x, y, z) -> multAssociative x y z)

multCommutative :: Int -> Int -> Bool
multCommutative x y = x * y == y * x

prop_multCommutative :: Property
prop_multCommutative = forAll (arbitrary :: Gen (Int, Int)) (uncurry multCommutative)

nonZeroInts :: Gen (Int, Int)
nonZeroInts = do
  x <- arbitrary `suchThat` (/= 0)
  y <- arbitrary `suchThat` (/= 0)
  return (x, y)

prop_quotRem :: Property
prop_quotRem = forAll nonZeroInts (\(x, y) -> quot x y * y + rem x y == x)

prop_divMod :: Property
prop_divMod = forAll nonZeroInts (\(x, y) -> div x y * y + mod x y == x)

twoPositiveInts :: Gen (Int, Int)
twoPositiveInts = do
  x <- arbitrary `suchThat` (>= 0)
  y <- arbitrary `suchThat` (>= 0)
  return (x, y)

threePositiveInts :: Gen (Int, Int, Int)
threePositiveInts = do
  (x, y) <- twoPositiveInts
  z <- arbitrary `suchThat` (>= 0)
  return (x, y, z)

-- (^) is not associative
prop_expNOTAssociative :: Property
prop_expNOTAssociative = forAll threePositiveInts (\(x, y, z) -> x ^ (y ^ z) == (x ^ y) ^ z)

-- (^) is not commutative
prop_expNOTCommutative :: Property
prop_expNOTCommutative = forAll twoPositiveInts (\(x, y) -> x ^ y == y ^ x)

prop_reverseList :: Property
prop_reverseList = forAll (arbitrary :: Gen [Int]) (\xs -> (reverse . reverse) xs == id xs)

prop_dollar :: Property
prop_dollar = forAll (arbitrary :: Gen Int) (\x -> id $ x == id x)

prop_compose :: Property
prop_compose = forAll (arbitrary :: Gen Int) (\x -> (id . id) x == id (id x))

twoLists :: Gen ([Int], [Int])
twoLists = do
  xs <- arbitrary
  ys <- arbitrary
  return (xs, ys)

prop_foldrConsConcatenation :: Property
prop_foldrConsConcatenation = forAll twoLists (\(xs, ys) -> foldr (:) xs ys == ys ++ xs)

prop_foldrConcatenationConcat :: Property
prop_foldrConcatenationConcat = forAll (arbitrary :: Gen [[Int]]) (\x -> foldr (++) [] x == concat x)

intAndList :: Gen ([Int], Int)
intAndList = do
  xs <- arbitrary
  n <- arbitrary
  return (xs, n)

prop_TakeThenLengthIsNOTn :: Property
prop_TakeThenLengthIsNOTn = forAll intAndList (\(xs, n) -> length (take n xs) == n)

prop_roundTrip :: Property
prop_roundTrip = forAll (arbitrary :: Gen String) (\x -> read (show x) == x)

-- Idempotence

twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

capitalizeIdempotent :: String -> Bool
capitalizeIdempotent x = (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

sortIdempotent :: Ord a => [a] -> Bool
sortIdempotent x = (sort x == twice sort x) && (sort x == fourTimes sort x)

prop_capitalizeIdempotent :: Property
prop_capitalizeIdempotent = forAll (arbitrary :: Gen String) capitalizeIdempotent

prop_sortIdempotent :: Property
prop_sortIdempotent = forAll (arbitrary :: Gen [Int]) sortIdempotent

return []

runTests :: IO Bool
runTests = $quickCheckAll
