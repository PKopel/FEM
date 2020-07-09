{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseRPN
  , parseToRPN
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Read                as TR
import           Data.Char
import           Control.Monad
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
  | (op ->- x && (not . isFunction) x)
    || (op -=- x && isLeftAssociative x)
    || (isFunction op && x /= "(")
  = shuntOp (op : out, ops) x
  | otherwise
  = (out, x : op : ops)
shuntOp (out, ops) op = (out, op : ops)

parseRPN :: Text -> Either Text DFunc
parseRPN text = case (foldM parse [] . T.words) text of
  Right [fun] -> Right fun
  Right _     -> Left "input is not a single function"
  Left  msg   -> Left msg

parse :: [DFunc] -> Text -> Either Text [DFunc]
parse (f : g : hs) "*"   = Right $ (f \* g) : hs
parse (f : g : hs) "+"   = Right $ (f \+ g) : hs
parse (f : g : hs) "-"   = Right $ (g \- f) : hs
parse (f : g : hs) "/"   = Right $ (g \/ f) : hs
parse (f : g : hs) "^"   = Right $ (g \^ f) : hs
parse (f     : hs) "ln"  = Right $ (log . f) : hs
parse (f     : hs) "sin" = Right $ (sin . f) : hs
parse (f     : hs) "cos" = Right $ (cos . f) : hs
parse (f     : hs) "tan" = Right $ (tan . f) : hs
parse (f     : hs) "cot" = Right $ (cot . f) : hs
parse fs           "e"   = Right $ const (exp 1) : fs
parse fs           "pi"  = Right $ const pi : fs
parse fs           x     = if isLetter $ T.head x
  then Right $ id : fs
  else case TR.double x of
    Right (n, _) -> Right $ (\_ -> n) : fs
    Left  msg    -> Left $ T.pack msg

