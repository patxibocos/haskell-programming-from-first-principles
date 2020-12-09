module Chapter15 where

import Data.Semigroup
import Test.QuickCheck

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

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity $ a <> b

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

type TwoAssoc = Two (Sum Int) (Product Int) -> Two (Sum Int) (Product Int) -> Two (Sum Int) (Product Int) -> Bool

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

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj _ <> BoolConj _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  BoolDisj _ <> BoolDisj _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

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

newtype Combine a b = Combine {unCombine :: a -> b}

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> (arbitrary :: (CoArbitrary a, Arbitrary b) => Gen (a -> b))

type CombineAssoc = Combine Int String -> Combine Int String -> Combine Int String -> Bool
