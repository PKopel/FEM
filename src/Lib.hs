module Lib
    ( someFunc
    ) where

type Func = (Double -> Double)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

partitions :: Double -> Double -> [Double] -> [Double]
partitions d start = map (\x -> start + d * x)

integral :: Func -> Double -> Double -> Double
integral f a b = ((b-a)/20 *) $ foldl1 (+) [ f x | x <- partitions d a [1,2..20]] where d = (b-a)/20

e :: Double -> Double -> Double -> Double
e xk d x
  | x < xk && x > xk - d = (x - xk + d) / d
  | x >= xk && x < xk + d = (xk + d - x) / d
  | otherwise = 0

e' :: Double -> Double -> Double -> Double
e' xk d x
  | x < xk && x > xk - d = 1 / d
  | x >= xk && x < xk + d = -1 / d
  | otherwise = 0

(#) :: Func -> Func -> Func
f # g = \x -> (f x) * (g x)

bij :: Func -> Func -> Func -> Double -> Double -> Double -> Double -> Double
bij a b c xi xj d k = let u = e xi d
                          u' = e' xi d
                          v = e xj d
                          v' = e' xj d
                          s = min xi xj
                          t = max xi xj
                      in k * (u # v) 0 + integral (a # u' # v') s t + integral (b # u' # v) s t + integral (c # u # v) s t

li :: Func -> Double -> Double -> Double -> Double
li f xi d k = integral (f # e xi d) (xi - d) (xi + d) - k * (e xi d 0)

thomas :: [Double] -> [Double] -> [Double] -> [Double] -> [Double]
thomas as bs cs ds = xs
              where n = length bs
                    a i = as !! i
                    b i = bs !! i
                    c i = cs !! i
                    d i = ds !! i
                    x i = xs !! i
                    b' i = bs' !! i
                    d' i = ds' !! i
                    bs' = b(0) : [b(i) - a(i)/b'(i-1) * c(i-1) | i <- [1..n-1]]
                    ds' = d(0) : [d(i) - a(i)/b'(i-1) * d'(i-1) | i <- [1..n-1]]
                    xs = reverse $ d'(n-1) / b'(n-1) : [(d'(i) - c(i) * x(i+1)) / b'(i) | i <- [n-2, n-3..0]]

bijs :: Int -> Double -> Func -> Func -> Func -> Double -> [Double]
bijs n nd a b c k =  [bij a b c (xks !! i) (xks !! (i + 1)) (1/nd) k | i <- [0..n-2]] ++ [0.0]
      where  xks = partitions (1/nd) 0 [0..nd]

biis :: Int -> Double -> Func -> Func -> Func -> Double -> [Double]
biis n nd a b c k =  [bij a b c (xks !! i) (xks !! i) (1/nd) k | i <- [0..n-2]] ++ [1.0]
      where  xks = partitions (1/nd) 0 [0..nd]

bjis :: Int -> Double -> Func -> Func -> Func -> Double -> [Double]
bjis n nd a b c k =  [bij a b c (xks !! (i + 1)) (xks !! i) (1/nd) k | i <- [0..n-1]]
      where  xks = partitions (1/nd) 0 [0..nd]

lis :: Int -> Double -> Func -> Double -> Double -> [Double]
lis n nd f k ur = [li f (xks !! i) (1/nd) k | i <- [0..n-2]] ++ [ur]
      where  xks =  partitions (1/nd) 0 [0..nd]

solve :: Func -> Func -> Func -> Func -> Int -> Double -> Double -> Double -> Double -> [Double]
solve a b c f n nd k l ur = thomas (bijs n nd a b c k) (biis n nd a b c k) (bjis n nd a b c k) (lis n nd f l ur)