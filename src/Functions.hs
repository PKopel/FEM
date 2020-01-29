module Functions
  ( parseRPN
  )
where

import           Data.Char
import           Data.List
import           Utils

parseRPN :: String -> [Func]
parseRPN = foldl parse [] . words

parse :: [Func] -> String -> [Func]
parse (f : g : hs) "*"   = (f #* g) : hs
parse (f : g : hs) "+"   = (f #+ g) : hs
parse (f : g : hs) "-"   = (g #- f) : hs
parse (f : g : hs) "/"   = (g #/ f) : hs
parse (f : g : hs) "^"   = (g #^ f) : hs
parse (f     : hs) "ln"  = (log . f) : hs
parse (f     : hs) "sin" = (sin . f) : hs
parse (f     : hs) "cos" = (cos . f) : hs
parse (f     : hs) "tan" = (tan . f) : hs
parse (f     : hs) "cot" = (cot . f) : hs
parse fs           "e"   = (\_ -> exp 1) : fs
parse fs           "pi"  = (\_ -> pi) : fs
parse fs x = if isLetter $ head x then id : fs else (\_ -> read x) : fs