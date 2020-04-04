module FEM
  ( solve
  )
where

import           Utils

e :: Double -> Double -> Double -> Double
e xk d x | x < xk && x > xk - d  = (x - xk + d) / d
         | x >= xk && x < xk + d = (xk + d - x) / d
         | otherwise             = 0

e' :: Double -> Double -> Double -> Double
e' xk d x | x < xk && x > xk - d = 1 / d
          | x > xk && x < xk + d = -1 / d
          | otherwise            = 0

bij :: Func -> Func -> Func -> Double -> Double -> Double -> Double -> Double
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


li :: Func -> Double -> Double -> Double -> Double
li f xi d l =
  let (s, t) = if xi == 0 || xi == 1
        then (max 0 (xi - d), min 1 (xi + d))
        else (xi - d, xi + d)
  in  integral (f #* e xi d) s t - (l * (e xi d 0))

bijs :: Int -> Double -> Func -> Func -> Func -> Double -> [Double]
bijs n nd a b c k =
  [ bij a b c (xks !! i) (xks !! (i + 1)) (1 / nd) k | i <- [0 .. n - 2] ]
    ++ [0.0]
  where xks = partitions (1 / nd) 0 [0 .. nd]

biis :: Int -> Double -> Func -> Func -> Func -> Double -> [Double]
biis n nd a b c k =
  [ bij a b c (xks !! i) (xks !! i) (1 / nd) k | i <- [0 .. n - 1] ] ++ [1.0]
  where xks = partitions (1 / nd) 0 [0 .. nd]

bjis :: Int -> Double -> Func -> Func -> Func -> Double -> [Double]
bjis n nd a b c k =
  [ bij a b c (xks !! (i + 1)) (xks !! i) (1 / nd) k | i <- [0 .. n - 1] ]
  where xks = partitions (1 / nd) 0 [0 .. nd]

lis :: Int -> Double -> Func -> Double -> Double -> [Double]
lis n nd f k ur = [ li f (xks !! i) (1 / nd) k | i <- [0 .. n - 1] ] ++ [ur]
  where xks = partitions (1 / nd) 0 [0 .. nd]

solve :: Func
  -> Func
  -> Func
  -> Func
  -> Int
  -> Double
  -> Double
  -> Double
  -> Double
  -> [(Double, Double)]
solve a b c f n nd k l ur =
  let bijList = bijs n nd a b c k
      biiList = biis n nd a b c k
      bjiList = bjis n nd a b c k
      liList  = lis n nd f l ur
  in  (partitions (1 / nd) 0 [0 .. nd])
        `zip` (solveM bijList biiList bjiList liList)
