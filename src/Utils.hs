{-# LANGUAGE BangPatterns,OverloadedStrings #-}

module Utils
  ( ignStr
  , integral
  , solveThomas
  , partitions
  , (->-)
  , (-=-)
  , isFunction
  , isOperator
  , isLeftAssociative
  , (\*)
  , (\/)
  , (\+)
  , (\-)
  , (\^)
  , cot
  , EdgeCond(..)
  , Func
  , DFunc
  , Operator
  )
where

import           Data.Text                      ( Text )

type Func a = a -> a
type DFunc = Func Double
type Operator = Text


data EdgeCond t = EC { a :: Func t
                     , b :: Func t
                     , c :: Func t
                     , f :: Func t
                     , k :: t
                     , l :: t
                     , ur :: t}

ignStr :: Text -> IO ()
ignStr _ = return ()

(->-) :: Operator -> Operator -> Bool
op1 ->- op2 = comparePrecedence op1 op2 == GT

(-=-) :: Operator -> Operator -> Bool
op1 -=- op2 = comparePrecedence op1 op2 == EQ

comparePrecedence :: Operator -> Operator -> Ordering
comparePrecedence "(" _   = LT
comparePrecedence _   "(" = LT
comparePrecedence "^" _   = GT
comparePrecedence _   "^" = LT
comparePrecedence "*" "/" = EQ
comparePrecedence "/" "*" = EQ
comparePrecedence "*" _   = GT
comparePrecedence _   "*" = LT
comparePrecedence "/" _   = GT
comparePrecedence _   "/" = LT
comparePrecedence "+" "-" = EQ
comparePrecedence "-" "+" = EQ
comparePrecedence "+" _   = GT
comparePrecedence _   "+" = LT
comparePrecedence "-" _   = GT
comparePrecedence _   "-" = LT
comparePrecedence _   _   = EQ


isOperator :: Text -> Bool
isOperator =
  (`elem` ["+", "-", "*", "/", "^", "sin", "cos", "ln", "tan", "cot", "(", ")"])

isLeftAssociative :: Operator -> Bool
isLeftAssociative = (`elem` ["+", "-", "*", "/"])

isFunction :: Operator -> Bool
isFunction = (`elem` ["sin", "cos", "ln", "tan", "cot"])

partitions :: (Fractional a) => a -> a -> [Int] -> [a]
partitions !d !start = map (\x -> start + d * fromIntegral x)

integral :: (Fractional a) => Func a -> a -> a -> a
integral f !a !b = d * sum [ f x | x <- partitions d a [1 .. 999] ]
  where !d = (b - a) / 999

(\*) :: (Fractional a) => Func a -> Func a -> Func a
f \* g = (*) <$> f <*> g

(\/) :: (Fractional a) => Func a -> Func a -> Func a
f \/ g = (/) <$> f <*> g

(\+) :: (Fractional a) => Func a -> Func a -> Func a
f \+ g = (+) <$> f <*> g

(\-) :: (Fractional a) => Func a -> Func a -> Func a
f \- g = (-) <$> f <*> g

(\^) :: (Floating a) => Func a -> Func a -> Func a
f \^ g = (**) <$> f <*> g

cot :: (Floating a) => a -> a
cot x = 1 / tan x

solveThomas :: (Fractional a) => [a] -> [a] -> [a] -> [a] -> [a]
solveThomas as bs cs rs = reverse xs
 where
  n = length bs
  a i = as !! (i - 1)
  b i = bs !! i
  c i = cs !! i
  r i = rs !! i
  x i = xs !! i
  c' i = cs' !! i
  r' i = rs' !! i
  d i = b i - (a i * c' (i - 1))
  cs' = c 0 / b 0 : [ c i / d i | i <- [1 .. n - 2] ]
  rs' = r 0 / b 0 : [ (r i - (a i * r' (i - 1))) / d i | i <- [1 .. n - 1] ]
  xs  = last rs' : [ r' i - (c' i * x (n - 2 - i)) | i <- [n - 2, n - 3 .. 0] ]
