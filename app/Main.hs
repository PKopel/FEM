{-# Language LambdaCase #-}
module Main where

import           System.Environment
import           CLI
import           GUI

main :: IO ()
main = getArgs >>= \case
  ("-cli" : rest) -> cli rest
  ("-gui" : rest) -> gui rest
  _               -> putStr "usage: 'fem -cli|-gui [<filename>]"
