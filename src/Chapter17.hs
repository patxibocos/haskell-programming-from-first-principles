module Chapter17 where

import Data.List (elemIndex)

-- Exercises: Lookups

added :: Maybe Integer
added = (+ 3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

y1 :: Maybe Integer
y1 = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z1 :: Maybe Integer
z1 = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y1 <*> z1

x2 :: Maybe Int
x2 = elemIndex 3 [1, 2, 3, 4, 5]

y2 :: Maybe Int
y2 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x2 <*> y2

xs :: [Integer]
xs = [1, 2, 3]

ys :: [Integer]
ys = [4, 5, 6]

x3 :: Maybe Integer
x3 = lookup 3 $ zip xs ys

y3 :: Maybe Integer
y3 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x3 <*> y3

-- Exercise: Identity Instance

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

-- Exercise: Constant Instance

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant b) = Constant b

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant a1) <*> (Constant a2) = Constant $ mappend a1 a2

-- Exercise: Fixer Upper

fix1 :: Maybe [Char]
fix1 = const <$> Just "Hello" <*> pure "World"

fix2 :: Maybe (Integer, Integer, [Char], [Integer])
fix2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
