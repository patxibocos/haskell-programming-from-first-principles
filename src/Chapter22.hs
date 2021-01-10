module Chapter22 where

import Control.Applicative (liftA2)
import Data.Char

-- Short Exercise: Warming Up

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (flip (,)) cap rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  c <- cap
  r <- rev
  return (c, r)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = cap >>= \c -> rev >>= \r -> return (c, r)
