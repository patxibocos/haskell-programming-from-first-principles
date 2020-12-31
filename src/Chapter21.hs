module Chapter21 where

import Chapter15 (Identity (Identity), Optional (Nada, Only))
import Chapter17 (Constant (Constant))
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 21.12 Chapter Exercises

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where (=-=) = eq

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

type Triple = (Int, Int, [Int])

runTests :: IO ()
runTests = do
  quickBatch $ traversable (undefined :: (Identity Triple))
  quickBatch $ traversable (undefined :: (Constant Int Triple))
  quickBatch $ traversable (undefined :: (Optional Triple))
