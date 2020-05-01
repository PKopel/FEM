module Parser
  ( parseRPN
  , parseToRPN
  )
where

import           Data.Char
import           Data.List
import           Utils

parseToRPN :: String -> String
parseToRPN = unwords . shunt ([], []) . words

shunt :: ([String], [Operator]) -> [String] -> [String]
shunt (out, ops) [] = (reverse out) ++ ops
shunt (out, ops) (x : rest) | isOperator x = shunt (shuntOp (out, ops) x) rest
                            | otherwise    = shunt (x : out, ops) rest

shuntOp :: ([String], [Operator]) -> Operator -> ([String], [Operator])
shuntOp (out, ("(" : ops)) ")" = (out, ops)
shuntOp (out, (op : ops) ) ")" = shuntOp (op : out, ops) ")"
shuntOp (out, (op : ops)) x
  | (op `hasGtPrecedence` x && (not . isFunction) x)
    || (op `hasEqPrecedence` x && isLeftAssociative x)
    || (isFunction op && x /= "(")
  = shuntOp (op : out, ops) x
  | otherwise
  = (out, x : op : ops)
shuntOp (out, ops) op = (out, op : ops)

parseRPN :: String -> [Func Double]
parseRPN = foldl parse [] . words

parse :: [Func Double] -> String -> [Func Double]
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
