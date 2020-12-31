module Chapter18 where

import Chapter15 (Identity (Identity))
import Chapter16 (List, Sum (First, Second))
import Chapter17 (flatten)
import Control.Monad
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Short Exercise: Either Monad

instance Applicative (Sum a) where
  pure = Second
  (First a) <*> _ = First a
  (Second f) <*> s = fmap f s

instance Monad (Sum a) where
  (First a) >>= _ = First a
  (Second b) >>= f = f b

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

-- 18.7 Chapter Exercises

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

data PhhhbbtttEither b a = Left a | Right b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Chapter18.Left a) = Chapter18.Left $ f a
  fmap _ (Chapter18.Right b) = Chapter18.Right b

instance Applicative (PhhhbbtttEither b) where
  pure = Chapter18.Left
  (Chapter18.Left f) <*> a = fmap f a
  (Chapter18.Right b) <*> _ = Chapter18.Right b

instance Monad (PhhhbbtttEither b) where
  (Chapter18.Left a) >>= f = f a
  (Chapter18.Right b) >>= _ = Chapter18.Right b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    elements [Chapter18.Left a, Chapter18.Right b]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

instance Monad Identity where
  (Identity a) >>= f = f a

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

instance Monad List where
  as >>= f = flatten $ fmap f as

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) f = liftM2 (:) (f x) (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType ms = meh ms id

type Triple = (Int, Double, Float)

runTests :: IO ()
runTests = do
  quickBatch $ monad (undefined :: (Sum String Triple))
  quickBatch $ monad (undefined :: (Nope Triple))
  quickBatch $ monad (undefined :: (PhhhbbtttEither String Triple))
  quickBatch $ monad (undefined :: (Identity Triple))
  quickBatch $ monad (undefined :: (List Triple))
