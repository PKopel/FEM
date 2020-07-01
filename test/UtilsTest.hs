module UtilsTest
    (testUtils
    ) where

import           Test.QuickCheck
import           Utils

data FF' = FF' { f  :: DFunc
               , f' :: DFunc}

instance Show FF' where
  show ff' = "function and its derivative"

instance Arbitrary FF' where
  arbitrary = oneof [ return $ FF' (**2) (/2)
                    , return $ FF' (1/) ((0.5/) . (**2))
                    , return $ FF' log (1/)]

prop_solveThomas :: Int -> Property
prop_solveThomas n =
  2 < n
    ==> let ones   = replicate n 1
            zeroes = replicate n 0
        in  solveThomas zeroes ones zeroes ones == ones

prop_partitions :: Double -> Double -> [Int] -> Bool
prop_partitions d start list =
  partitions d start list == [ d * fromIntegral x + start | x <- list ]

prop_integral :: FF' -> Bool
prop_integral (FF' f f') = abs (integral f' 0 1 - (f 1 - f 0)) < 0.1

testUtils :: IO ()
testUtils = do
  putStrLn "\n*** Testing solveThomas... ***"
  quickCheck prop_solveThomas
  putStrLn "\n*** Testing partitions... ***"
  quickCheck prop_partitions
  putStrLn "\n*** Testing integral... ***"
  quickCheck prop_integral