import Test.QuickCheck
import Utils

prop_solveM :: Int -> Bool
prop_solveM x = solveM zeroes ones zeroes ones == ones
        where n = if x > 2 then x else 3
              ones = take n $ repeat 1
              zeroes = take n $ repeat 0

main :: IO ()
main = do
  putStrLn "\n*** Testing solveM... ***"
  quickCheck prop_solveM