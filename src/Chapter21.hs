module Chapter21 where

import Chapter15 (Identity (Identity), Optional (Nada, Only), Three (Three), Two (Two))
import Chapter16 (List (Cons, Nil), Three' (Three'))
import Chapter17 (Constant (Constant))
import Chapter20 (Four' (Four'))
import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 21.12 Chapter Exercises

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

instance Foldable (Chapter17.Constant a) where
  foldMap _ _ = mempty

instance Traversable (Chapter17.Constant a) where
  traverse _ (Chapter17.Constant a) = pure (Chapter17.Constant a)

instance (Arbitrary a) => Arbitrary (Chapter17.Constant a b) where
  arbitrary = Chapter17.Constant <$> arbitrary

instance (Eq a) => EqProp (Chapter17.Constant a b) where (=-=) = eq

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Only a) = Only $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Only a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Only a) = Only <$> f a

instance (Eq a) => EqProp (Optional a) where (=-=) = eq

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a as) = f a <> foldMap f as

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance Traversable (Two a) where
  traverse f (Two a b) = Two a <$> f b

instance Traversable (Three' a) where
  traverse f (Three' a b1 b2) = Three' a <$> f b1 <*> f b2

instance Functor (Four' a) where
  fmap f (Four' a b1 b2 b3) = Four' a (f b1) (f b2) (f b3)

instance Traversable (Four' a) where
  traverse f (Four' a b1 b2 b3) = liftA3 (Four' a) (f b1) (f b2) (f b3)

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
  (S x y) =-= (S p q) = property ((=-=) <$> x <*> p) .&. (y =-= q)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

type Triple = (Int, Int, [Int])

runTests :: IO ()
runTests = do
  quickBatch $ traversable (undefined :: (Identity Triple))
  quickBatch $ traversable (undefined :: (Chapter17.Constant Int Triple))
  quickBatch $ traversable (undefined :: (Optional Triple))
  quickBatch $ traversable (undefined :: (List Triple))
  quickBatch $ traversable (undefined :: (Three Int Int Triple))
  quickBatch $ traversable (undefined :: (Two Int Triple))
  quickBatch $ traversable (undefined :: (Three' Int Triple))
  quickBatch $ traversable (undefined :: (Four' Int Triple))
  quickBatch $ traversable (undefined :: (S Maybe Triple))
