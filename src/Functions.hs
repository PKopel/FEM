module Functions
(Func
,parseRPN
)where

import Data.List  
import Data.Char

type Func = (Double -> Double)
type Operator = (Double -> Double -> Double)
  
parseRPN :: String -> Func  
parseRPN = head . foldl parse [] . words

apply :: Func -> Func -> Operator -> Func
apply f g h = (\x -> h (f x) (g x))

parse :: [Func] -> String -> [Func]
parse (f:g:hs) "*" = (apply f g (*)):hs
parse (f:g:hs) "+" = (apply f g (+)):hs
parse (f:g:hs) "-" = (apply g f (-)):hs
parse (f:g:hs) "/" = (apply g f (/)):hs  
parse (f:g:hs) "^" = (apply g f (**)):hs  
parse (f:hs) "ln" = (log.f):hs  
parse fs "e" = (\_ -> exp 1):fs
parse fs x = if isLetter $ head x then id:fs else (\_ -> read x):fs  

