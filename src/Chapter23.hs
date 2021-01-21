{-# LANGUAGE InstanceSigs #-}

module Chapter23 where

import System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  x -> error $ "intToDie got non 1-6 integer: " ++ show x

-- Exercises: Roll Your Own

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 []
  where
    go :: Int -> [Die] -> StdGen -> (Int, [Die])
    go sum dies gen
      | sum >= n = (length dies, dies)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (dies ++ [intToDie die]) nextGen

-- 23.6 Write State for yourself

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi {runMoi = \s -> let (a1, s1) = g s in (f a1, s1)}

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi {runMoi = \s -> (a, s)}
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi
      { runMoi = \s ->
          let (fa, s1) = f s
              (a, s2) = g s1
           in (fa a, s2)
      }

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g =
    Moi
      { runMoi = \s ->
          let (a, s1) = f s
           in runMoi (g a) s1
      }
