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
parse fs x
  | x == "e" = Right $ const (exp 1) : fs
  | x == "pi" = Right $ const pi : fs
  | isFunction x && length fs > 0 = Right $ parseFun fs x
  | isOperator x && length fs > 1 = Right $ parseOp fs x
  | isLetter $ T.head x = Right $ id : fs
  | otherwise = case TR.double x of
    Right (n, _) -> Right $ (\_ -> n) : fs
    Left  msg    -> Left $ T.pack msg

parseFun :: [DFunc] -> Text -> [DFunc]
parseFun (f : hs) "ln"  = (log . f) : hs
parseFun (f : hs) "sin" = (sin . f) : hs
parseFun (f : hs) "cos" = (cos . f) : hs
parseFun (f : hs) "tan" = (tan . f) : hs
parseFun (f : hs) "cot" = (cot . f) : hs

parseOp :: [DFunc] -> Text -> [DFunc]
parseOp (f : g : hs) "*" = (f \* g) : hs
parseOp (f : g : hs) "+" = (f \+ g) : hs
parseOp (f : g : hs) "-" = (g \- f) : hs
parseOp (f : g : hs) "/" = (g \/ f) : hs
parseOp (f : g : hs) "^" = (g \^ f) : hs
