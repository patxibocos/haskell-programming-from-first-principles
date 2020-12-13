{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Chapter15 where

import Data.Semigroup
import Test.QuickCheck (Arbitrary, CoArbitrary, Gen, arbitrary, elements, frequency, quickCheckAll)

-- Exercise: Optional Monoid

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  (Only a1) <> (Only a2) = Only $ a1 <> a2
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  Nada <> Nada = Nada

-- 15.11 Madness

type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]

-- Exercise: Maybe Another Monoid

newtype First' a = First' {getFirst' :: Optional a}
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada

instance Semigroup (First' a) where
  (First' Nada) <> (First' Nada) = First' Nada
  (First' Nada) <> (First' (Only b)) = First' (Only b)
  (First' (Only a)) <> _ = First' (Only a)

onlyGen :: Arbitrary a => Gen (Optional a)
onlyGen = Only <$> arbitrary

optionalGen :: Arbitrary a => Gen (Optional a)
optionalGen = frequency [(1, return Nada), (10, onlyGen)]

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = optionalGen

firstGen :: Arbitrary a => Gen (First' a)
firstGen = First' <$> arbitrary

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

-- 15.15 Chapter exercises

-- Semigroup exercises

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

prop_trivAssoc :: TrivAssoc
prop_trivAssoc = semigroupAssoc

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity $ a <> b

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

prop_identityAssoc :: IdentityAssoc
prop_identityAssoc = semigroupAssoc

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

type TwoAssoc = Two (Sum Int) (Product Int) -> Two (Sum Int) (Product Int) -> Two (Sum Int) (Product Int) -> Bool

prop_twoAssoc :: TwoAssoc
prop_twoAssoc = semigroupAssoc

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

type ThreeAssoc =
  Three (Sum Int) (Product Int) (Sum Int) ->
  Three (Sum Int) (Product Int) (Sum Int) ->
  Three (Sum Int) (Product Int) (Sum Int) ->
  Bool

prop_threeAssoc :: ThreeAssoc
prop_threeAssoc = semigroupAssoc

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type FourAssoc =
  Four (Sum Int) (Product Int) (Sum Int) (Product Int) ->
  Four (Sum Int) (Product Int) (Sum Int) (Product Int) ->
  Four (Sum Int) (Product Int) (Sum Int) (Product Int) ->
  Bool

prop_fourAssoc :: FourAssoc
prop_fourAssoc = semigroupAssoc

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj _ <> BoolConj _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

prop_boolConjAssoc :: BoolConjAssoc
prop_boolConjAssoc = semigroupAssoc

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  BoolDisj _ <> BoolDisj _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

prop_boolDisjAssoc :: BoolDisjAssoc
prop_boolDisjAssoc = semigroupAssoc

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  a@(Snd _) <> _ = a
  _ <> b = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

type OrAssoc = Or String Int -> Or String Int -> Or String Int -> Bool

prop_orAssoc :: OrAssoc
prop_orAssoc = semigroupAssoc

newtype Combine a b = Combine {unCombine :: a -> b}

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

newtype Comp a = Comp {unComp :: a -> a}

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  s@(Success _) <> _ = s
  _ <> s@(Success _) = s
  (Failure f1) <> (Failure f2) = Failure $ f1 <> f2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Failure a, Success b]

type ValidationAssoc = Validation String Int -> Validation String Int -> Validation String Int -> Bool

prop_validationAssoc :: ValidationAssoc
prop_validationAssoc = semigroupAssoc

-- Monoid exercises

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a <> mempty == a

instance Monoid Trivial where
  mempty = Trivial

prop_trivialLeftIdentity :: Trivial -> Bool
prop_trivialLeftIdentity = monoidLeftIdentity

prop_trivialRightIdentity :: Trivial -> Bool
prop_trivialRightIdentity = monoidRightIdentity

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

prop_identityLeftIdentity :: Identity String -> Bool
prop_identityLeftIdentity = monoidLeftIdentity

prop_identityRightIdentity :: Identity String -> Bool
prop_identityRightIdentity = monoidRightIdentity

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

prop_twoLeftIdentity :: Two (Sum Int) String -> Bool
prop_twoLeftIdentity = monoidLeftIdentity

prop_twoRightIdentity :: Two (Sum Int) String -> Bool
prop_twoRightIdentity = monoidRightIdentity

instance Monoid BoolConj where
  mempty = BoolConj True

prop_boolConjLeftIdentity :: BoolConj -> Bool
prop_boolConjLeftIdentity = monoidLeftIdentity

prop_boolConjRightIdentity :: BoolConj -> Bool
prop_boolConjRightIdentity = monoidRightIdentity

instance Monoid BoolDisj where
  mempty = BoolDisj False

prop_boolDisjLeftIdentity :: BoolDisj -> Bool
prop_boolDisjLeftIdentity = monoidLeftIdentity

prop_boolDisjRightIdentity :: BoolDisj -> Bool
prop_boolDisjRightIdentity = monoidRightIdentity

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty

newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance (Semigroup a, Semigroup s) => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem (f <> g)

instance (Monoid a, Semigroup s) => Monoid (Mem s a) where
  mempty = Mem (mempty,) -- Tuple sectioning, same as (\s -> (mempty, s))

return []

runTests :: IO Bool
runTests = $quickCheckAll
