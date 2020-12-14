{-# LANGUAGE TemplateHaskell #-}

module Chapter16 where

import Chapter15 (Four (Four), Identity (Identity), Three (Three), Two (Two))
import Test.QuickCheck

-- Exercises: Heavy Lifting

a :: [Int]
a = fmap (+ 1) $ read "[1]" :: [Int]

b :: Maybe [[Char]]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c :: Integer -> Integer
c = (* 2) . (\x -> x - 2)

d :: Integer -> [Char]
d = ((return '1' ++) . show) . (\x -> [x, 1 .. 3])

e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = fmap (read . ("123" ++) . show) ioi
   in fmap (* 3) changed

-- 16.10 Exercises: Instances of Func

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap (g . f) x == fmap g (fmap f x)

instance Functor Identity where -- Orphan instance but ok for this use case
  fmap f (Identity x) = Identity (f x)

prop_identityFunctorIdentity :: Identity String -> Bool
prop_identityFunctorIdentity = functorIdentity

prop_identityFunctorCompose :: Fun Int String -> Fun String Char -> Identity Int -> Bool
prop_identityFunctorCompose (Fun _ f) (Fun _ g) = functorCompose f g

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

prop_pairFunctorIdentity :: Identity String -> Bool
prop_pairFunctorIdentity = functorIdentity

prop_pairFunctorCompose :: Fun Int String -> Fun String Char -> Pair Int -> Bool
prop_pairFunctorCompose (Fun _ f) (Fun _ g) = functorCompose f g

instance Functor (Two a) where -- Orphan instance but ok for this use case
  fmap f (Two x y) = Two x (f y)

prop_twoFunctorIdentity :: Two Int String -> Bool
prop_twoFunctorIdentity = functorIdentity

prop_twoFunctorCompose :: Fun Int String -> Fun String Char -> Two String Int -> Bool
prop_twoFunctorCompose (Fun _ f) (Fun _ g) = functorCompose f g

instance Functor (Three a b) where -- Orphan instance but ok for this use case
  fmap f (Three x y z) = Three x y (f z)

prop_threeFunctorIdentity :: Three Int String Float -> Bool
prop_threeFunctorIdentity = functorIdentity

prop_threeFunctorCompose :: Fun Int String -> Fun String Char -> Three Double Float Int -> Bool
prop_threeFunctorCompose (Fun _ f) (Fun _ g) = functorCompose f g

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

prop_three'FunctorIdentity :: Three' Int String -> Bool
prop_three'FunctorIdentity = functorIdentity

prop_three'FunctorCompose :: Fun Int String -> Fun String Char -> Three' Double Int -> Bool
prop_three'FunctorCompose (Fun _ f) (Fun _ g) = functorCompose f g

instance Functor (Four a b c) where -- Orphan instance but ok for this use case
  fmap f (Four w x y z) = Four w x y (f z)

prop_FourFunctorIdentity :: Four Int String Float Double -> Bool
prop_FourFunctorIdentity = functorIdentity

prop_FourFunctorCompose :: Fun Int String -> Fun String Char -> Four String Double Float Int -> Bool
prop_FourFunctorCompose (Fun _ f) (Fun _ g) = functorCompose f g

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

prop_Four'FunctorIdentity :: Four' Int String -> Bool
prop_Four'FunctorIdentity = functorIdentity

prop_Four'FunctorCompose :: Fun Int String -> Fun String Char -> Four' String Int -> Bool
prop_Four'FunctorCompose (Fun _ f) (Fun _ g) = functorCompose f g

-- Exercise: Possibly

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers $ f x

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return LolNope), (10, return $ Yeppers x)]

prop_PossiblyFunctorIdentity :: Possibly String -> Bool
prop_PossiblyFunctorIdentity = functorIdentity

prop_PossiblyFunctorIdentityCompose :: Fun Int String -> Fun String Char -> Possibly Int -> Bool
prop_PossiblyFunctorIdentityCompose (Fun _ f) (Fun _ g) = functorCompose f g

-- Short Exercise

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second $ f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [First x, Second y]

prop_SumFunctorIdentity :: Sum String Int -> Bool
prop_SumFunctorIdentity = functorIdentity

prop_SumFunctorIdentityCompose :: Fun Int String -> Fun String Char -> Sum Char Int -> Bool
prop_SumFunctorIdentityCompose (Fun _ f) (Fun _ g) = functorCompose f g

return []

runTests :: IO Bool
runTests = $quickCheckAll
