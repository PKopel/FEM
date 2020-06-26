{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseRPN
  , parseToRPN
  )
where

import           Data.Text (Text)
import qualified Data.Text        as T
import qualified Data.Text.Read   as TR
import           Data.Char
import           Utils

parseToRPN :: Text -> Text
parseToRPN = T.unwords . shunt ([], []) . T.words

shunt :: ([Text], [Operator]) -> [Text] -> [Text]
shunt (out, ops) [] = reverse out ++ ops
shunt (out, ops) (x : rest) | isOperator x = shunt (shuntOp (out, ops) x) rest
                            | otherwise    = shunt (x : out, ops) rest

shuntOp :: ([Text], [Operator]) -> Operator -> ([Text], [Operator])
shuntOp (out, "(" : ops) ")" = (out, ops)
shuntOp (out, op : ops ) ")" = shuntOp (op : out, ops) ")"
shuntOp (out, op : ops) x
  | (op `hasGtPrecedence` x && (not . isFunction) x)
    || (op `hasEqPrecedence` x && isLeftAssociative x)
    || (isFunction op && x /= "(")
  = shuntOp (op : out, ops) x
  | otherwise
  = (out, x : op : ops)
shuntOp (out, ops) op = (out, op : ops)

parseRPN :: Text -> [Func Double]
parseRPN = foldl parse [] . T.words

parse :: [Func Double] -> Text -> [Func Double]
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
parse fs           "e"   = const (exp 1) : fs
parse fs           "pi"  = const pi : fs
parse fs x = if isLetter $ T.head x then id : fs else funx : fs
      where funx = (\_ -> case TR.double x of Right (n, _) -> n
                                              Left _ -> 1)
