module Chapter12 where

-- 12.5 Chapter Exercises

-- String processing

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a

replaceThe :: String -> String
replaceThe = unwords . go . words
  where
    go [] = []
    go (x : xs) = case notThe x of
      Nothing -> "a" : go xs
      (Just word) -> word : go xs

vowels :: [Char]
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel = (`elem` vowels)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    go (x : y : ys) = if x == "the" && isVowel (head y) then 1 else go (y : ys)
    go _ = 0

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel

-- Validate the word

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord w = if v > c then Nothing else Just $ Word' w
  where
    v = countVowels w
    c = fromIntegral (length w) - v

-- It's only Natural

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just $ buildNat i
  where
    buildNat 0 = Zero
    buildNat n = Succ $ buildNat (n - 1)

-- Small library for Maybe

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a Nothing = a

listToMaybe :: [a] -> Maybe a
listToMaybe (x : _) = Just x
listToMaybe _ = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes =
  foldr
    ( \a acc ->
        case a of
          (Just x) -> x : acc
          Nothing -> acc
    )
    []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr go (Just [])
  where
    go (Just x) (Just y) = Just (x : y)
    go _ _ = Nothing

-- Small library for Either

lefts' :: [Either a b] -> [a]
lefts' =
  foldr
    ( \a acc -> case a of
        (Left l) -> l : acc
        _ -> acc
    )
    []

rights' :: [Either a b] -> [b]
rights' =
  foldr
    ( \a acc -> case a of
        (Right r) -> r : acc
        _ -> acc
    )
    []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' e = (lefts' e, rights' e)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' aToC _ (Left a) = aToC a
either' _ bToC (Right b) = bToC b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
