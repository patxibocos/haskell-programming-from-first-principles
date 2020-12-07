module Chapter15 where

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
