{-# LANGUAGE OverloadedStrings #-}

module ParserTest
    (testParser
    ) where

import           Test.QuickCheck
import           Parser

instance Arbitrary Text where
    arbitrary = 

prop_parseToRPN :: Text -> Bool
prop_parseToRPN text = 

testParser :: IO ()
testParser = do