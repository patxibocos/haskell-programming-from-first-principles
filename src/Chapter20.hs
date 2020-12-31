module Chapter20 where

import Chapter15 (Three (Three), Two (Two))
import Chapter16 (Three' (Three'))
import Data.Monoid
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Library Functions

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e = getAny . foldMap (\a -> Any $ a == e)

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = compare' min

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = compare' max

compare' :: (Foldable t) => (a -> a -> a) -> t a -> Maybe a
compare' f ta
  | null ta = Nothing
  | otherwise = Just $ foldr f (head . toList $ ta) ta

null' :: (Foldable t) => t a -> Bool
null' = getAll . foldMap (\_ -> All False)

length :: (Foldable t) => t a -> Int
length = getSum . foldMap (\_ -> Sum 1)

toList :: (Foldable t) => t a -> [a]
toList = foldMap (: [])

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a <> b) mempty

-- 20.6 Chapter Exercises

data Constant a b = Constant b deriving (Show)

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

instance (Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Foldable (Three' a) where
  foldMap f (Three' _ b1 b2) = f b1 <> f b2

data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ b1 b2 b3) = f b1 <> f b2 <> f b3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type Quintuple = (Int, Double, String, Rational, Integer)

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)

runTests :: IO ()
runTests = do
  quickBatch $ foldable (undefined :: (Constant Int Quintuple))
  quickBatch $ foldable (undefined :: (Two Int Quintuple))
  quickBatch $ foldable (undefined :: (Three Int String Quintuple))
  quickBatch $ foldable (undefined :: (Three' Int Quintuple))
  quickBatch $ foldable (undefined :: (Four' Int Quintuple))
