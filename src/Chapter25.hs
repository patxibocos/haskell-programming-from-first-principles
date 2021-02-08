{-# LANGUAGE InstanceSigs #-}

module Chapter25 where

newtype Compose f g a = Compose {getCompose :: f (g a)}
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- 25.4 Twinplicative

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ fmap (<*>) f <*> a

-- 25.6 Exercises: Compose Instances

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fg) = (foldMap . foldMap) f fg

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fg) = Compose <$> (traverse . traverse) f fg

-- And now for something completely different

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap ::
    (a -> b) ->
    (c -> d) ->
    p a c ->
    p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const $ f a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either a b = Left a | Right b

instance Bifunctor Chapter25.Either where
  bimap f _ (Chapter25.Left a) = Chapter25.Left $ f a
  bimap _ g (Chapter25.Right b) = Chapter25.Right $ g b
