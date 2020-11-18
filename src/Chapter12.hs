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

-- Small library for Maybe
