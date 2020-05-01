module FEM
  ( solve
  )
where

import           Utils

e :: (Fractional a, Ord a) => a -> a -> a -> a
e xk d x | x < xk && x > xk - d  = (x - xk + d) / d
         | x >= xk && x < xk + d = (xk + d - x) / d
         | otherwise             = 0

e' :: (Fractional a, Ord a) => a -> a -> a -> a
e' xk d x | x < xk && x > xk - d = 1 / d
          | x > xk && x < xk + d = -1 / d
          | otherwise            = 0

bij :: (Fractional a, Ord a)
  => Func a
  -> Func a
  -> Func a
  -> a
  -> a
  -> a
  -> a
  -> a
bij a b c xi xj d k =
  let
    u      = e xi d
    u'     = e' xi d
    v      = e xj d
    v'     = e' xj d
    (s, t) = if xi == xj
      then (max 0 (xi - d), min 1 (xi + d))
      else (min xi xj, max xi xj)
  in
    ((-1) * k * (u #* v) 0)
    - integral (a #* u' #* v') s t
    + integral (b #* u' #* v)  s t
    + integral (c #* u #* v)   s t


li :: (Fractional a, Ord a) => Func a -> a -> a -> a -> a
li f xi d l =
  let (s, t) = if xi == 0 || xi == 1
        then (max 0 (xi - d), min 1 (xi + d))
        else (xi - d, xi + d)
  in  integral (f #* e xi d) s t - (l * (e xi d 0))

bijs :: (Fractional a, Ord a) => Int -> Func a -> Func a -> Func a -> a -> [a]
bijs n a b c k =
  [ bij a b c (xks !! i) (xks !! (i + 1)) (1.0 / fromIntegral n) k | i <- [0 .. n - 2] ] ++ [0.0]
  where xks = partitions (1.0 / fromIntegral n) 0 [0 .. n]

biis :: (Fractional a, Ord a) => Int -> Func a -> Func a -> Func a -> a -> [a]
biis n a b c k =
  [ bij a b c (xks !! i) (xks !! i) (1.0 / fromIntegral n) k | i <- [0 .. n - 1] ] ++ [1.0]
  where xks = partitions (1.0 / fromIntegral n) 0 [0 .. n]

bjis :: (Fractional a, Ord a) => Int -> Func a -> Func a -> Func a -> a -> [a]
bjis n a b c k =
  [ bij a b c (xks !! (i + 1)) (xks !! i) (1.0 / fromIntegral n) k | i <- [0 .. n - 1] ]
  where xks = partitions (1.0 / fromIntegral n) 0 [0 .. n]

lis :: (Fractional a, Ord a) => Int -> Func a -> a -> a -> [a]
lis n f k ur =
  [ li f (xks !! i) (1.0 / fromIntegral n) k | i <- [0 .. n - 1] ] ++ [ur]
  where xks = partitions (1.0 / fromIntegral n) 0 [0 .. n]

solve :: (Fractional a, Ord a)
  => Func a
  -> Func a
  -> Func a
  -> Func a
  -> Int
  -> a
  -> a
  -> a
  -> [(a, a)]
solve a b c f n k l ur =
  let bijList = bijs n a b c k
      biiList = biis n a b c k
      bjiList = bjis n a b c k
      liList  = lis n f l ur
  in  zip 
    (partitions (1.0 / fromIntegral n) 0 [0 .. n])
    (solveM bijList biiList bjiList liList)
