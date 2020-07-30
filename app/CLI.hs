{-# LANGUAGE OverloadedStrings #-}

module CLI where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           FEM
import           Parser
import           System.IO
import           Utils

import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy

cli :: [String] -> IO ()
cli args = do
  (a, b, c, f, n, k, l, ur, fName) <- if not (null args)
    then openFile (head args) ReadMode >>= fileInput (T.pack $ head args) ignStr
    else fileInput "chart" TIO.putStrLn stdin
  toFile def (T.unpack fName <> ".svg")
    $ plot (line "u(x)" [solve (EC a b c f k l ur) n])

fileInput
  :: Text
  -> (Text -> IO ())
  -> Handle
  -> IO (DFunc, DFunc, DFunc, DFunc, Int, Double, Double, Double, Text)
fileInput fileName msgFunc handle = do
  a  <- hReadFunc handle msgFunc "enter a(x):"
  b  <- hReadFunc handle msgFunc "enter b(x):"
  c  <- hReadFunc handle msgFunc "enter c(x):"
  f  <- hReadFunc handle msgFunc "enter f(x):"
  n  <- hReadNum handle msgFunc "enter n:"
  k  <- hReadNum handle msgFunc "enter k:"
  l  <- hReadNum handle msgFunc "enter l:"
  ur <- hReadNum handle msgFunc "enter ur:"
  hClose handle
  return (a, b, c, f, n, k, l, ur, T.takeWhile (/= '.') fileName)

errorMsg :: Text
errorMsg = "wrong input, "

hReadNum :: (Num a, Read a) => Handle -> (Text -> IO ()) -> Text -> IO a
hReadNum handle msgFunc msg =
  msgFunc msg >> reads <$> hGetLine handle >>= checkNum msg

checkNum :: (Num a, Read a) => Text -> [(a, String)] -> IO a
checkNum _   [(n, "")] = return n
checkNum msg _         = hReadNum stdin TIO.putStrLn $ errorMsg <> msg

hReadFunc :: Handle -> (Text -> IO ()) -> Text -> IO DFunc
hReadFunc handle msgFunc msg =
  msgFunc msg >> parseRPN . parseToRPN <$> TIO.hGetLine handle >>= checkFunc msg

checkFunc :: Text -> Either Text DFunc -> IO DFunc
checkFunc _ (Right f) = return f
checkFunc msg (Left err) =
  hReadFunc stdin TIO.putStrLn $ errorMsg <> err <> msg
