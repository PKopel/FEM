{-# LANGUAGE BangPatterns #-}

module FEM
  ( solve
  )
where

import           Utils

e :: (Fractional a, Ord a) => a -> a -> Func a
e xk dx x | x < xk && x > xk - dx  = (x - xk + dx) / dx
          | x >= xk && x < xk + dx = (xk + dx - x) / dx
          | otherwise              = 0

e' :: (Fractional a, Ord a) => a -> a -> Func a
e' xk dx x | x < xk && x > xk - dx  = 1 / dx
           | x >= xk && x < xk + dx = -1 / dx
           | otherwise             = 0

bij :: (Fractional a, Ord a)
  => Func a
  -> Func a
  -> Func a
  -> a
  -> a
  -> a
  -> a
  -> a
bij a b c !xi !xj !dx !k = result
 where
  u        = e xi dx
  u'       = e' xi dx
  v        = e xj dx
  v'       = e' xj dx
  (!s, !t) = if xi == xj
    then (max 0 (xi - dx), min 1 (xi + dx))
    else (min xi xj, max xi xj)
  !result =
    (-1)
      * k
      * (u #* v) 0
      - integral (a #* u' #* v') s t
      + integral (b #* u' #* v)  s t
      + integral (c #* u #* v)   s t


li :: (Fractional a, Ord a) => Func a -> a -> a -> a -> a
li f !xi !dx !l = result
 where
  (!s, !t) = if xi == 0 || xi == 1
    then (max 0 (xi - dx), min 1 (xi + dx))
    else (xi - dx, xi + dx)
  !result = integral (f #* e xi dx) s t - (l * e xi dx 0)

bijs :: (Fractional a, Ord a) => Func a -> Func a -> Func a -> a -> a -> [a] -> [a]
bijs a b c !dx k xks =
  [ bij a b c xi (xi + dx) dx k | !xi <- init $ init xks ] ++ [0.0]

biis :: (Fractional a, Ord a) => Func a -> Func a -> Func a -> a -> a -> [a] -> [a]
biis a b c !dx k xks = [ bij a b c xi xi dx k | !xi <- init xks ] ++ [1.0]

bjis :: (Fractional a, Ord a) => Func a -> Func a -> Func a -> a -> a -> [a] -> [a]
bjis a b c !dx k xks = [ bij a b c (xi + dx) xi dx k | !xi <- init xks ]

lis :: (Fractional a, Ord a) => Func a -> a -> a -> a -> [a] -> [a]
lis f !dx k ur xks = [ li f xi dx k | !xi <- init xks ] ++ [ur]

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
  let !dx     = 1.0 / fromIntegral n
      xks     = partitions dx 0 [0 .. n]
      bijList = bijs a b c dx k xks
      biiList = biis a b c dx k xks
      bjiList = bjis a b c dx k xks
      liList  = lis f dx l ur xks
  in  zip xks (solveM bijList biiList bjiList liList)
