{-# LANGUAGE FlexibleInstances #-}

module Chapter11 where

import Data.Char
import Data.List

-- Exercises: Vehicles

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Integer deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir 15000

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = fmap isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- Exercises: Logic Goats

class TooMany a where tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (a, _) = a > 42

instance TooMany (Int, Int) where
  tooMany (a, b) = a + b > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = tooMany (a + b)

-- Exercise: Programmers

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem,
    lang :: ProgLang
  }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux,
    OpenBSDPlusNevermindJustBSDStill,
    Mac,
    Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [ Haskell,
    Agda,
    Idris,
    PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = o, lang = l} | o <- allOperatingSystems, l <- allLanguages]

-- Exercises: Binary Tree

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = x : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = inorder left ++ x : inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = (postorder left ++ postorder right) ++ [x]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left x right) = foldTree f (f x (foldTree f acc left)) right

-- 11.18 Chapter Exercises

shift :: Char -> Char -> Char
shift base c
  | c == ' ' = ' '
  | c `elem` ['A' .. 'Z'] = chr $ 65 + (ord c + ord base) `mod` 26
  | otherwise = error "character must be an uppercase letter"

vigenere :: String -> String -> String
vigenere _ "" = ""
vigenere "" phrase = phrase
vigenere keyword x = zipWith shift (keyGenerator keyword x) x

keyGenerator :: String -> String -> String
keyGenerator keyword = go (concat $ repeat keyword)
  where
    go k (' ' : xs) = ' ' : go k xs
    go (k : ks) (_ : xs) = k : go ks xs
    go _ _ = ""

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf a@(x : xs) (y : ys) = if x == y then isSubseqOf xs ys else isSubseqOf a ys
isSubseqOf [] _ = True
isSubseqOf _ _ = False

capitalizeWords :: String -> [(String, String)]
capitalizeWords = fmap (\w@(x : xs) -> (w, toUpper x : xs)) . words

capitalizeWord :: String -> String
capitalizeWord [] = ""
capitalizeWord (x : xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph p =
  unwords $
    foldl
      ( \acc a ->
          if mustCapitalize acc
            then acc ++ [capitalizeWord a]
            else acc ++ [a]
      )
      []
      (words p)
  where
    mustCapitalize [] = True
    mustCapitalize x = last (last x) == '.'

type Digit = Char

type Presses = Int

type Letters = [Char]

data DaPhone = DaPhone [(Digit, Letters)]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

daPhone :: DaPhone
daPhone =
  DaPhone
    [ ('1', "1"),
      ('2', "abc"),
      ('3', "def"),
      ('4', "ghi"),
      ('5', "jkl"),
      ('6', "mno"),
      ('7', "pqrs"),
      ('8', "tuv"),
      ('9', "wxyz"),
      ('*', "*^"),
      ('0', " +_"),
      ('#', "#.,")
    ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone@(DaPhone p) c
  | c `elem` ['A' .. 'Z'] = ('*', 1) : reverseTaps phone (chr (ord c + 32))
  | otherwise =
    foldr
      ( \a acc ->
          case elemIndex c (snd a) of
            Just i -> [(fst a, i + 1)]
            Nothing -> acc
      )
      []
      p

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . fmap snd
