module Chapter7 where

functionC x y = case (x > y) of
  True -> x
  False -> y

ifEvenAdd2 n = case (even n) of
  True -> n + 2
  False -> n

nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = fst $ divMod x 10
    d = snd $ xLast `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d
  where
    xLast = x `div` 100
    d = xLast `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y c = case c of
  False -> x
  True -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y c
  | c == False = x
  | c == True = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show

--print (roundTrip' 4 :: Int)
