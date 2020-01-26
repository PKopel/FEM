module Utils
    ( integral
    , thomas
    , partitions
    , (#*)
    , (#/)
    , (#+)
    , (#-)
    , (#^)
    , cot
    , Func
    ) where

type Func = (Double -> Double)

partitions :: Double -> Double -> [Double] -> [Double]
partitions d start = map (\x -> start + d * x)

integral :: Func -> Double -> Double -> Double
integral f a b = ((b-a)/30 *) $ foldl1 (+) [ f x | x <- partitions d a [1,2..30]] where d = (b-a)/30

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
cot x = 1/(tan x)

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