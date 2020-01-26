module Main where

import System.IO
import Functions
import FEM

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

main :: IO ()
main = do
    a <- readFunc "enter a(x):"
    b <- readFunc "enter b(x):"
    c <- readFunc "enter c(x):"
    f <- readFunc "enter f(x):"
    (n,nd) <- readN "enter n:"
    k <- readNum "enter k:"
    l <- readNum "enter l:"
    ur <- readNum "enter ur:"
    toFile def "u_chart.svg" $ do
        layout_title .= "Solution"
        plot (line "u(x)" [solve a b c f n nd k l ur])


readN :: String -> IO (Int,Double)
readN msg = putStrLn msg >> (\n -> (read n,read n)) <$> getLine

readNum :: String -> IO Double
readNum msg = putStrLn msg >> read <$> getLine

readFunc :: String -> IO Func
readFunc msg = putStrLn msg >> parseRPN <$> getLine
