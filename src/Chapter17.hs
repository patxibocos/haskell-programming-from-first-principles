module Chapter17 where

import Chapter15 (Four (Four), Three (Three), Two (Two))
import Chapter16 (Four' (Four'), List (Cons, Nil), Pair (Pair), Three' (Three'))
import Control.Applicative (liftA3)
import Data.List (elemIndex)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

-- List Applicative Exercise

instance Applicative List where -- Orphan instance but ok for this use case
  pure = flip Cons Nil -- Same as pure a = Cons a Nil
  fs <*> as = flatten $ fmap (`fmap` as) fs

flatten :: List (List a) -> List a
flatten Nil = Nil
flatten (Cons x Nil) = x
flatten (Cons x y) = x `combine` flatten y

combine :: List a -> List a -> List a
combine Nil y = y
combine x Nil = x
combine (Cons x xs) y = Cons x (xs `combine` y)

instance (Eq a) => EqProp (List a) where (=-=) = eq

-- ZipList Applicative Exercise

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons a as) = Cons a (take' (n -1) as)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take' 3000 l
      ys' =
        let (ZipList' l) = ys
         in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Applicative ZipList' where
  pure a = ZipList' $ Cons a Nil
  (ZipList' fs) <*> (ZipList' as) = ZipList' (zipLists fs as)

zipLists :: List (a -> b) -> List a -> List b
zipLists Nil _ = Nil
zipLists _ Nil = Nil
zipLists (Cons f Nil) (Cons a as) = Cons (f a) (f <$> as)
zipLists (Cons f fs) as@(Cons a Nil) = Cons (f a) (fs <*> as)
zipLists (Cons f fs) (Cons a as) = Cons (f a) (zipLists fs as)

-- Exercise: Variations on Either

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Success f) <*> (Success a) = Success $ f a
  (Failure e1) <*> (Failure e2) = Failure $ e1 `mappend` e2
  (Success _) <*> (Failure e) = Failure e
  (Failure e) <*> (Success _) = Failure e

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    elements [Success a, Failure e]

instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq

-- 17.9 Chapter Exercises

instance Applicative Pair where
  pure a = Pair a a
  (Pair f1 f2) <*> (Pair x y) = Pair (f1 x) (f2 y)

instance (Eq a) => EqProp (Pair a) where (=-=) = eq

instance (Monoid a) => Applicative (Two a) where
  pure b = Two mempty b
  (Two x f) <*> (Two x' y) = Two (x <> x') (f y)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Monoid a) => Applicative (Three' a) where
  pure b = Three' mempty b b
  (Three' a f1 f2) <*> (Three' a' b1 b2) = Three' (a <> a') (f1 b1) (f2 b2)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (Four a b c f) <*> (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

instance (Monoid a) => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  (Four' a1 a2 a3 f) <*> (Four' a1' a2' a3' b) = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f b)

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

-- Combinations

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

triple :: (String, String, String)
triple = ("a", "b", "c")

runTests :: IO ()
runTests = do
  quickBatch $ applicative (Cons triple Nil)
  quickBatch $ applicative (ZipList' $ Cons triple Nil)
  quickBatch $ applicative (Success triple :: Validation String (String, String, String))
  quickBatch $ applicative (Pair triple triple)
  quickBatch $ applicative (Two triple triple)
  quickBatch $ applicative (Three triple triple triple)
  quickBatch $ applicative (Three' triple triple triple)
  quickBatch $ applicative (Four triple triple triple triple)
  quickBatch $ applicative (Four' triple triple triple triple)
