module Main where

import           System.Environment
import           CLI
import           GUI

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("-cli" : rest) -> cli rest
    ("-gui" : rest) -> gui rest
    _               -> putStr "usage: 'fem -cli|-gui [<filename>]"
