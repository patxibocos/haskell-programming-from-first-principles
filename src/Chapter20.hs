module Chapter20 where

import Data.Monoid

-- Exercises: Library Functions

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e = getAny . foldMap (\a -> Any $ a == e)

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = compare' min

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = compare' max

compare' :: (Foldable t) => (a -> a -> a) -> t a -> Maybe a
compare' f ta
  | null ta = Nothing
  | otherwise = Just $ foldr f (head . toList $ ta) ta

null' :: (Foldable t) => t a -> Bool
null' = getAll . foldMap (\_ -> All False)

length :: (Foldable t) => t a -> Int
length = getSum . foldMap (\_ -> Sum 1)

toList :: (Foldable t) => t a -> [a]
toList = foldMap (: [])

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a <> b) mempty
