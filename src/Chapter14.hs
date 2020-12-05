module Chapter14 where

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

halfIdentityProp :: Property
halfIdentityProp = forAll (arbitrary :: Gen Double) (\x -> halfIdentity x == x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

listOrderedProp :: Property
listOrderedProp = forAll (arbitrary :: Gen [Int]) (listOrdered . sort)
