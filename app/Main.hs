module Main where

import           FEM
import           Functions
import           System.Environment
import           System.IO
import           Utils
import           Data.List

import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy

main :: IO ()
main = do
    args <- getArgs
    print args
    (a,b,c,f,n,nd,k,l,ur,fName) <-  if length args > 0 then fileInput $ head args else cmdInput
    toFile def (fName ++ ".svg") $ do
        layout_title .= "Solution"
        plot (line "u(x)" [solve a b c f n nd k l ur])

cmdInput :: IO (Func,Func,Func,Func,Int,Double,Double,Double,Double,String)
cmdInput = do
        a <- readFunc "enter a(x):"
        b <- readFunc "enter b(x):"
        c <- readFunc "enter c(x):"
        f <- readFunc "enter f(x):"
        (n,nd) <- readN "enter n:"
        k <- readNum "enter k:"
        l <- readNum "enter l:"
        ur <- readNum "enter ur:"
        return (a,b,c,f,n,nd,k,l,ur,"chart")

fileInput :: String -> IO (Func,Func,Func,Func,Int,Double,Double,Double,Double,String)
fileInput fileName = do
                handle <- openFile fileName ReadMode
                a <- hReadFunc handle "enter a(x):"
                b <- hReadFunc handle "enter b(x):"
                c <- hReadFunc handle "enter c(x):"
                f <- hReadFunc handle "enter f(x):"
                (n,nd) <- hReadN handle "enter n:"
                k <- hReadNum handle "enter k:"
                l <- hReadNum handle "enter l:"
                ur <- hReadNum handle "enter ur:"
                hClose handle
                return (a,b,c,f,n,nd,k,l,ur,takeWhile (/= '.') fileName)

errorMsg = "wrong input, "

readN :: String -> IO (Int,Double)
readN msg = putStrLn msg >> (\n -> (reads n,reads n)) <$> getLine >>= checkN msg

hReadN :: Handle -> String -> IO (Int,Double)
hReadN handle msg = (\n -> (reads n,reads n)) <$> hGetLine handle >>= checkN msg

checkN :: String -> ([(Int,String)],[(Double,String)]) -> IO (Int,Double)
checkN _ ([(n,"")],[(nd,"")]) = return (n,nd)
checkN msg _                  = readN $ errorMsg ++ msg

readNum :: String -> IO Double
readNum msg = putStrLn msg >> reads <$> getLine >>= checkNum msg

hReadNum :: Handle -> String -> IO Double
hReadNum handle msg = reads <$> hGetLine handle >>= checkNum msg

checkNum :: String -> [(Double,String)] -> IO Double
checkNum _ [(n,"")] = return n
checkNum msg _      = readNum $ errorMsg ++ msg

readFunc :: String -> IO Func
readFunc msg = putStrLn msg >> parseRPN <$> getLine >>= checkFunc msg

hReadFunc :: Handle -> String -> IO Func
hReadFunc handle msg = parseRPN <$> hGetLine handle >>= checkFunc msg

checkFunc :: String -> [Func] -> IO Func
checkFunc _ [f] = return f
checkFunc msg _ = readFunc $ errorMsg ++ msg