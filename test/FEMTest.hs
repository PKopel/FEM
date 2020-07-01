module FEMTest
    (testFEM
    ) where

import           Test.QuickCheck
import           FEM

prop_e :: Double -> Double -> Double -> Bool
prop_e a b c = case abs a - c of
    0 -> e a b c == 1
    y -> if y > b then e a b c == 0 else e a b c < 1

prop_e' :: Double -> Double -> Double -> Bool
prop_e' a b c = case abs a - c of
    0 -> e' a b c == 0
    y -> if y > b then abs $ e' a b c == 1 else e' a b c == 0

prop_bij :: Double -> Double -> Double -> Bool
prop_bij a b c = if abs (a - b) < 2 * c 
                 then bijabc /= 0
                 else bijabc == 0
                 where bijabc = bij id id id a b c 1

prop_li :: Double -> Double -> Bool
prop_li a b = li id a b 0 /= 0

testFEM :: IO ()
testFEM = do
  putStrLn "\n*** Testing e... ***"
  quickCheck prop_e
  putStrLn "\n*** Testing e'... ***"
  quickCheck prop_e'
  putStrLn "\n*** Testing bij... ***"
  quickCheck prop_bij
  putStrLn "\n*** Testing li... ***"
  quickCheck prop_li