{-# LANGUAGE FlexibleInstances #-}
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

-- 16.17 Chapter exercises

-- Rearrange the arguments to the type constructor of the datatype so the Functor instance works.

data Sum' b a
  = First' a
  | Second' b

instance Functor (Sum' e) where
  fmap f (First' a) = First' (f a)
  fmap f (Second' b) = Second' b

data Company a c b
  = DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a
  = L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.

data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor x) = Bloor $ f x
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Finance, Desk x, Bloor y]

prop_QuantFunctorIdentity :: Quant String Int -> Bool
prop_QuantFunctorIdentity = functorIdentity

prop_QuantFunctorIdentityCompose :: Fun Int String -> Fun String Char -> Quant Char Int -> Bool
prop_QuantFunctorIdentityCompose (Fun _ f) (Fun _ g) = functorCompose f g

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

prop_KFunctorIdentity :: K String Int -> Bool
prop_KFunctorIdentity = functorIdentity

prop_KFunctorIdentityCompose :: Fun Int String -> Fun String Char -> K Char Int -> Bool
prop_KFunctorIdentityCompose (Fun _ f) (Fun _ g) = functorCompose f g

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' x)) = Flip (K' $ f x)

instance (Arbitrary b) => Arbitrary (Flip K' a b) where
  arbitrary = Flip . K' <$> arbitrary

prop_FlipFunctorIdentity :: Flip K' Int String -> Bool
prop_FlipFunctorIdentity = functorIdentity

prop_FlipFunctorIdentityCompose :: Fun Int String -> Fun String Char -> Flip K' Char Int -> Bool
prop_FlipFunctorIdentityCompose (Fun _ f) (Fun _ g) = functorCompose f g

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary

prop_EvilGoateeConstFunctorIdentity :: EvilGoateeConst Int String -> Bool
prop_EvilGoateeConstFunctorIdentity = functorIdentity

prop_EvilGoateeConstFunctorIdentityCompose :: Fun Int String -> Fun String Char -> EvilGoateeConst Char Int -> Bool
prop_EvilGoateeConstFunctorIdentityCompose (Fun _ f) (Fun _ g) = functorCompose f g

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut $ fmap f x

instance (Arbitrary a) => Arbitrary (LiftItOut Maybe a) where
  arbitrary = do
    x <- arbitrary
    elements [LiftItOut Nothing, LiftItOut $ Just x]

prop_LiftItOutGoateeConstFunctorIdentity :: LiftItOut Maybe String -> Bool
prop_LiftItOutGoateeConstFunctorIdentity = functorIdentity

prop_LiftItOutGoateeConstFunctorIdentityCompose :: Fun Int String -> Fun String Char -> LiftItOut Maybe Int -> Bool
prop_LiftItOutGoateeConstFunctorIdentityCompose (Fun _ f) (Fun _ g) = functorCompose f g

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

instance (Arbitrary a) => Arbitrary (Parappa Maybe (Either String) a) where
  arbitrary = do
    x <- arbitrary
    elements [DaWrappa (Just x) (Right x), DaWrappa Nothing (Left "")]

prop_ParappaGoateeConstFunctorIdentity :: Parappa Maybe (Either String) String -> Bool
prop_ParappaGoateeConstFunctorIdentity = functorIdentity

prop_ParappaGoateeConstFunctorIdentityCompose :: Fun Int String -> Fun String Char -> Parappa Maybe (Either String) Int -> Bool
prop_ParappaGoateeConstFunctorIdentityCompose (Fun _ f) (Fun _ g) = functorCompose f g

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

instance (Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne Maybe Maybe a b) where
  arbitrary = IgnoringSomething <$> arbitrary <*> arbitrary

prop_IgnoreOneFunctorIdentity :: IgnoreOne Maybe Maybe String String -> Bool
prop_IgnoreOneFunctorIdentity = functorIdentity

prop_IgnoreOneFunctorIdentityCompose :: Fun Int String -> Fun String Char -> IgnoreOne Maybe Maybe Int Int -> Bool
prop_IgnoreOneFunctorIdentityCompose (Fun _ f) (Fun _ g) = functorCompose f g

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance (Arbitrary o, Arbitrary a, Arbitrary t) => Arbitrary (Notorious Maybe o a t) where
  arbitrary = Notorious <$> arbitrary <*> arbitrary <*> arbitrary

prop_NotoriousFunctorIdentity :: Notorious Maybe String String String -> Bool
prop_NotoriousFunctorIdentity = functorIdentity

prop_NotoriousFunctorIdentityCompose :: Fun Int String -> Fun String Char -> Notorious Maybe Int Int Int -> Bool
prop_NotoriousFunctorIdentityCompose (Fun _ f) (Fun _ g) = functorCompose f g

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Nil, Cons x Nil, Cons x y]

prop_ListFunctorIdentity :: List String -> Bool
prop_ListFunctorIdentity = functorIdentity

prop_ListFunctorIdentityCompose :: Fun Int String -> Fun String Char -> List Int -> Bool
prop_ListFunctorIdentityCompose (Fun _ f) (Fun _ g) = functorCompose f g

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = do
    x <- arbitrary
    g1 <- arbitrary
    g2 <- arbitrary
    g3 <- arbitrary
    elements [NoGoat, OneGoat x, MoreGoats g1 g2 g3]

prop_GoatLordFunctorIdentity :: GoatLord String -> Bool
prop_GoatLordFunctorIdentity = functorIdentity

prop_GoatLordFunctorIdentityCompose :: Fun Int String -> Fun String Char -> GoatLord Int -> Bool
prop_GoatLordFunctorIdentityCompose (Fun _ f) (Fun _ g) = functorCompose f g

data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read fa) = Read (f . fa)

return []

runTests :: IO Bool
runTests = $quickCheckAll
