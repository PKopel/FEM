module Utils
  ( integral
  , solveM
  , partitions
  , (#*)
  , (#/)
  , (#+)
  , (#-)
  , (#^)
  , cot
  , Func
  )
where

type Func = (Double -> Double)

partitions :: Double -> Double -> [Double] -> [Double]
partitions d start = map (\x -> start + d * x)

integral :: Func -> Double -> Double -> Double
integral f a b = ((b - a) / 999 *)
  $ foldl1 (+) [ f x | x <- partitions d a [1, 2 .. 999] ]
  where d = (b - a) / 999

(#*) :: Func -> Func -> Func
f #* g = (*) <$> f <*> g

(#/) :: Func -> Func -> Func
f #/ g = (/) <$> f <*> g

(#+) :: Func -> Func -> Func
f #+ g = (+) <$> f <*> g

(#-) :: Func -> Func -> Func
f #- g = (-) <$> f <*> g

(#^) :: Func -> Func -> Func
f #^ g = (**) <$> f <*> g

cot :: Floating a => a -> a
cot x = 1 / (tan x)

solveM :: Fractional g => [g] -> [g] -> [g] -> [g] -> [g]
solveM as bs cs rs = reverse xs
  where
    n = length bs
    a i = as !! (i - 1)
    b i = bs !! i
    c i = cs !! i
    r i = rs !! i
    x i = xs !! i
    c' i = cs' !! i
    r' i = rs' !! i
    d i = (b i) - (a i * c' (i - 1))
    cs' = c 0 / b 0 : [(c i) / d i | i <- [1 .. n -2] ]
    rs' = r 0 / b 0 : [(r i - (a i * r' (i - 1))) / (d i) | i <- [1 .. n -1] ]
    xs = last rs' : [ (r' i) - (c' i * x (n - 2 - i)) | i <- [n - 2, n - 3 .. 0] ]