module Chapter28 where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- Exercise: Benchmark Practice

bumpIt :: (Num a, Num b) => (a, b) -> (a, b)
bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream where stream = iterate (+ 1) 0

insertMap :: Int -> ()
insertMap i = let _ = M.insert i 0 m in ()

insertSet :: Int -> ()
insertSet i = let _ = S.insert i s in ()

deleteAtMap :: Int -> ()
deleteAtMap i = let _ = M.deleteAt i m in ()

deleteAtSet :: Int -> ()
deleteAtSet i = let _ = S.deleteAt i s in ()

-- Exercise: Vector

data Boxed a = Boxed a

unboxed :: VU.Unbox a => [a] -> VU.Vector a
unboxed = VU.fromList

boxed :: [a] -> V.Vector (Boxed a)
boxed = V.fromList . fmap Boxed

-- 28.10 Chapter Exercises

-- Difference List

newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL (const [])
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL (const [x])
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList xs = unDL xs []
{-# INLINE toList #-}

infixr 9 `cons`

cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)
{-# INLINE cons #-}

infixl 9 `snoc`

snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x :))
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n -1) (n : xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n -1) (singleton n `append` xs)

-- A simple queue

data Queue a = Queue
  { enqueue :: [a],
    dequeue :: [a]
  }
  deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a (Queue xs ys) = Queue (a : xs) ys

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue xs (y : ys)) = Just (y, Queue xs ys)
pop (Queue xs []) = let (rx : rxs) = reverse xs in Just (rx, Queue [] rxs)

main :: IO ()
main =
  defaultMain
    [ bench "insert map" $ whnf insertMap 10000,
      bench "insert set" $ whnf insertSet 10000,
      bench "deleteAt map" $ whnf deleteAtMap 9999,
      bench "deleteAt set" $ whnf deleteAtSet 9999,
      bench "unboxed vector" $ whnf unboxed ([1 .. 1000000] :: [Int]),
      bench "boxed vector" $ whnf boxed ([1 .. 1000000] :: [Int]),
      bench "concat list" $ whnf schlemiel 123456,
      bench "concat dlist" $ whnf constructDlist 123456
    ]
