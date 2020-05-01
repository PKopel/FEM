module Main where

import           FEM
import           Parser
import           System.Environment
import           System.IO
import           Utils
import           Data.List

import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy

type DFunc = Func Double

main :: IO ()
main = do
  args                             <- getArgs
  (a, b, c, f, n, k, l, ur, fName) <- if length args > 0
    then openFile (head args) ReadMode >>= fileInput (head args) ignStr
    else fileInput "chart" putStrLn stdin
  toFile def (fName ++ ".svg") $ do
    plot (line "u(x)" [solve a b c f n k l ur])

fileInput :: String
  -> (String -> IO ())
  -> Handle
  -> IO
       ( DFunc
       , DFunc
       , DFunc
       , DFunc
       , Int
       , Double
       , Double
       , Double
       , String
       )
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
  return (a, b, c, f, n, k, l, ur, takeWhile (/= '.') fileName)

errorMsg = "wrong input, "

hReadNum :: (Num a, Read a) => Handle -> (String -> IO ()) -> String -> IO a
hReadNum handle msgFunc msg =
  msgFunc msg >> reads <$> hGetLine handle >>= checkNum msg

checkNum :: (Num a, Read a) => String -> [(a, String)] -> IO a
checkNum _   [(n, "")] = return n
checkNum msg _         = hReadNum stdin putStrLn $ errorMsg ++ msg

hReadFunc :: Handle -> (String -> IO ()) -> String -> IO DFunc
hReadFunc handle msgFunc msg =
  msgFunc msg >> (parseRPN . parseToRPN) <$> hGetLine handle >>= checkFunc msg

checkFunc :: String -> [DFunc] -> IO DFunc
checkFunc _   [f] = return f
checkFunc msg _   = hReadFunc stdin putStrLn $ errorMsg ++ msg
