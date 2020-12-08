module Chapter15 where

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
onlyGen = do
  a <- arbitrary
  return $ Only a

optionalGen :: Arbitrary a => Gen (Optional a)
optionalGen = frequency [(1, return Nada), (10, onlyGen)]

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = optionalGen

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- arbitrary
  return $ First' a

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen
