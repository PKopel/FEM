{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module CLI where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           FEM
import           Parser
import           Utils
import           Files

import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy

cli :: [String] -> IO ()
cli args = do
  (ec, n, fName) <- parseFile args >>= \case
    Right values -> checkInput (T.pack $ head args) values
    Left  msg    -> putStrLn msg >> checkInput "chart" Map.empty
  toFile def (T.unpack fName <> ".svg") $ plot (line "u(x)" [solve ec n])

checkInput :: Text -> Map Text Text -> IO (EdgeCond Double, Int, Text)
checkInput fileName values = do
  a  <- checkFunc "enter a(x):" $ getFunc "a(x)"
  b  <- checkFunc "enter b(x):" $ getFunc "b(x)"
  c  <- checkFunc "enter c(x):" $ getFunc "c(x)"
  f  <- checkFunc "enter f(x):" $ getFunc "f(x)"
  n  <- checkNum "enter n:" $ getNum "n"
  k  <- checkNum "enter k:" $ getNum "k"
  l  <- checkNum "enter l:" $ getNum "l"
  ur <- checkNum "enter ur:" $ getNum "ur"
  return (EC a b c f k l ur, n, T.takeWhile (/= '.') fileName)
 where
  getFunc key = parseRPN . parseToRPN <$> Map.lookup key values
  getNum key = reads <$> T.unpack <$> Map.lookup key values

errorMsg :: Text
errorMsg = "wrong input, "

readNum :: (Num a, Read a) => Text -> IO a
readNum msg = TIO.putStrLn msg >> Just . reads <$> getLine >>= checkNum msg

checkNum :: (Num a, Read a) => Text -> Maybe ([(a, String)]) -> IO a
checkNum _   (Just [(n, "")]) = return n
checkNum msg _                = readNum $ errorMsg <> msg

readFunc :: Text -> IO DFunc
readFunc msg =
  TIO.putStrLn msg
    >>  Just
    .   parseRPN
    .   parseToRPN
    <$> TIO.getLine
    >>= checkFunc msg

checkFunc :: Text -> Maybe (Either Text DFunc) -> IO DFunc
checkFunc _   (Just (Right f  )) = return f
checkFunc msg (Just (Left  err)) = readFunc $ errorMsg <> err <> msg
checkFunc msg Nothing            = readFunc msg
