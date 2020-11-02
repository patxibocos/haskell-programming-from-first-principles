module Chapter9 where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr filterDate []
  where
    filterDate (DbDate time) xs = xs ++ [time]
    filterDate _ xs = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filterNumber []
  where
    filterNumber (DbNumber number) xs = xs ++ [number]
    filterNumber _ xs = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr filterRecent (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))
  where
    filterRecent (DbDate time) recent = if time > recent then time else recent
    filterRecent _ recent = recent

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sumNumber 0
  where
    sumNumber (DbNumber n) acc = n + acc
    sumNumber _ acc = acc

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / fromIntegral (length $ filterDbNumber db)
