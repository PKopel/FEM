{-# LANGUAGE OverloadedStrings #-}

module Files where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
import           Data.Attoparsec.Text
import           System.IO

parseFile :: [FilePath] -> IO (Either String (Map Text Text))
parseFile (file : _) = do
  handle   <- openFile file ReadMode
  contents <- TIO.hGetContents handle
  hClose handle
  return $ parseContents contents
parseFile [] = return $ Left "no file name"

parseContents :: Text -> Either String (Map Text Text)
parseContents contents =
  Map.fromList <$> parseOnly (many' parseLine <* endOfInput) contents

parseLine :: Parser (Text, Text)
parseLine =
  (,)
    <$> (   skipSpace
        *>  takeTill ((||) <$> isHorizontalSpace <*> (== ':'))
        <*  skipSpace
        <*  char ':'
        <?> "key"
        )
    <*> (skipSpace *> takeTill isEndOfLine <* endOfLine <?> "value")
