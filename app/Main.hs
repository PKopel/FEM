module Main where

import           FEM
import           Parser
import           System.Environment
import           System.IO
import           Utils
import           Data.List

import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy

main :: IO ()
main = do
  args <- getArgs
  (a, b, c, f, n, nd, k, l, ur, fName) <- if length args > 0
    then openFile (head args) ReadMode >>= fileInput (head args) ignStr
    else fileInput "chart" putStrLn stdin
  toFile def (fName ++ ".svg") $ do
    plot (line "u(x)" [solve a b c f n nd k l ur])

ignStr :: String -> IO ()
ignStr _ = return ()

fileInput:: String
  -> (String -> IO ())
  -> Handle
  -> IO ( Func, Func, Func, Func, Int, Double, Double, Double, Double, String)
fileInput fileName msgFunc handle = do
  a       <- hReadFunc handle msgFunc "enter a(x):"
  b       <- hReadFunc handle msgFunc "enter b(x):"
  c       <- hReadFunc handle msgFunc "enter c(x):"
  f       <- hReadFunc handle msgFunc "enter f(x):"
  (n, nd) <- hReadN handle msgFunc "enter n:"
  k       <- hReadNum handle msgFunc "enter k:"
  l       <- hReadNum handle msgFunc "enter l:"
  ur      <- hReadNum handle msgFunc "enter ur:"
  hClose handle
  return (a, b, c, f, n, nd, k, l, ur, takeWhile (/= '.') fileName)

errorMsg = "wrong input, "

hReadN :: Handle -> (String -> IO ()) -> String -> IO (Int, Double)
hReadN handle msgFunc msg =
  msgFunc msg >> (\n -> (reads n, reads n)) <$> hGetLine handle >>= checkN msg

checkN :: String -> ([(Int, String)], [(Double, String)]) -> IO (Int, Double)
checkN _   ([(n, "")], [(nd, "")]) = return (n, nd)
checkN msg _                       = hReadN stdin putStrLn $ errorMsg ++ msg

hReadNum :: Handle -> (String -> IO ()) -> String -> IO Double
hReadNum handle msgFunc msg =
  msgFunc msg >> reads <$> hGetLine handle >>= checkNum msg

checkNum :: String -> [(Double, String)] -> IO Double
checkNum _   [(n, "")] = return n
checkNum msg _         = hReadNum stdin putStrLn $ errorMsg ++ msg

hReadFunc :: Handle -> (String -> IO ()) -> String -> IO Func
hReadFunc handle msgFunc msg =
  msgFunc msg >> (parseRPN . parseToRPN) <$> hGetLine handle >>= checkFunc msg

checkFunc :: String -> [Func] -> IO Func
checkFunc _   [f] = return f
checkFunc msg _   = hReadFunc stdin putStrLn $ errorMsg ++ msg
