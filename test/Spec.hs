import           Test.QuickCheck
import           Utils

prop_solveM :: Int -> Property
prop_solveM n =
  2 < n
    ==> let ones   = take n $ repeat 1
            zeroes = take n $ repeat 0
        in  solveM zeroes ones zeroes ones == ones

prop_partitions :: Double -> Double -> [Double] -> Bool
prop_partitions d start list =
  partitions d start list == [ d * x + start | x <- list ]

prop_integral :: Double -> Property
prop_integral p =
  p >= 1 ==> abs ((integral (\x -> x ** p) 0 1) - (1 / (p + 1))) < 0.1

main :: IO ()
main = do
  putStrLn "\n*** Testing solveM... ***"
  quickCheck prop_solveM
  putStrLn "\n*** Testing partitions... ***"
  quickCheck prop_partitions
  putStrLn "\n*** Testing integral... ***"
  quickCheck prop_integral
