module UtilsTest
    (testUtils
    ) where

import           Test.QuickCheck
import           Utils

data FF' = FF' DFunc DFunc String

instance Show FF' where
  show (FF' _ _ s) = s

instance Arbitrary FF' where
  arbitrary = oneof [ return $ FF' (**2) (*2) "x^2"
                    , return $ FF' ((1/) . (+1))  (((-1)/) . (**2) . (+1)) "1/(x+1)"
                    , return $ FF' (log . (+1)) ((1/) . (+1)) "log(x+1)"]

prop_solveThomas :: Int -> Property
prop_solveThomas n =
  2 < n
    ==> let ones   = replicate n 1 :: [Double]
            zeroes = replicate n 0 :: [Double]
        in  solveThomas zeroes ones zeroes ones == ones

prop_partitions :: Double -> Double -> [Int] -> Bool
prop_partitions d start list =
  partitions d start list == [ d * fromIntegral x + start | x <- list ]

prop_integral :: FF' -> Bool
prop_integral (FF' f f' _) = abs (integral f' 0 1 - (f 1 - f 0)) < 0.1

testUtils :: IO ()
testUtils = do
  putStrLn "\n*** Testing solveThomas... ***"
  quickCheck prop_solveThomas
  putStrLn "\n*** Testing partitions... ***"
  quickCheck prop_partitions
  putStrLn "\n*** Testing integral... ***"
  quickCheck prop_integral