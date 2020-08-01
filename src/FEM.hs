{-# LANGUAGE BangPatterns #-}

module FEM
  ( solve
  , e
  , e'
  , bij
  , li
  )
where

import           Utils

e :: (Fractional a, Ord a) => a -> a -> Func a
e xk dx x | x < xk && x > xk - dx  = (x - xk + dx) / dx
          | x >= xk && x < xk + dx = (xk + dx - x) / dx
          | otherwise              = 0

e' :: (Fractional a, Ord a) => a -> a -> Func a
e' xk dx x | x < xk && x > xk - dx = 1 / dx
           | x > xk && x < xk + dx = -1 / dx
           | otherwise             = 0

bij :: (Fractional a, Ord a) => EdgeCond a -> a -> a -> a -> a
bij ec !xi !xj !dx = result
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
      * (k ec)
      * (u \* v) 0
      - integral ((a ec) \* u' \* v') s t
      + integral ((b ec) \* u' \* v)  s t
      + integral ((c ec) \* u \* v)   s t


li :: (Fractional a, Ord a) => EdgeCond a -> a -> a -> a
li ec !xi !dx = result
 where
  (!s, !t) = if xi == 0 || xi == 1
    then (max 0 (xi - dx), min 1 (xi + dx))
    else (xi - dx, xi + dx)
  !result = integral ((f ec) \* e xi dx) s t - ((l ec) * e xi dx 0)

bijs :: (Fractional a, Ord a) => EdgeCond a -> a -> [a] -> [a]
bijs ec !dx xks = [ bij ec xi (xi + dx) dx | !xi <- init $ init xks ] ++ [0.0]

biis :: (Fractional a, Ord a) => EdgeCond a -> a -> [a] -> [a]
biis ec !dx xks = [ bij ec xi xi dx | !xi <- init xks ] ++ [1.0]

bjis :: (Fractional a, Ord a) => EdgeCond a -> a -> [a] -> [a]
bjis ec !dx xks = [ bij ec (xi + dx) xi dx | !xi <- init xks ]

lis :: (Fractional a, Ord a) => EdgeCond a -> a -> [a] -> [a]
lis ec !dx xks = [ li ec xi dx | !xi <- init xks ] ++ [ur ec]

solve :: (Fractional a, Ord a) => EdgeCond a -> Int -> [(a, a)]
solve ec n =
  let !dx     = 1.0 / fromIntegral n
      xks     = partitions dx 0 [0 .. n]
      bijList = bijs ec dx xks
      biiList = biis ec dx xks
      bjiList = bjis ec dx xks
      liList  = lis ec dx xks
  in  zip xks (solveThomas bijList biiList bjiList liList)
