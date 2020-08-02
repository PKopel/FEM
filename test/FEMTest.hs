module FEMTest
    (testFEM
    ) where

import           Test.QuickCheck
import           FEM
import Utils

simpleEC :: EdgeCond Double
simpleEC = EC id id id id 1 0 0

prop_e :: Double -> Double -> Double -> Property
prop_e a b c = b > 0 ==> case abs (a - c) of
    0 -> eabc == 1
    y -> if y >= b then eabc == 0 else eabc > 0 && eabc < 1
    where eabc = e a b c

prop_e' :: Double -> Double -> Double -> Property
prop_e' a b c = b > 0 ==> case abs (a - c) of
    0 -> e'abc == 0
    y -> if y < b then abs e'abc == abs (1/b) else e'abc == 0
    where e'abc = e' a b c

prop_bij :: Double -> Double -> Double -> Property
prop_bij a b c = c > 0 ==> if abs (a - b) < 2 * c 
                 then bijabc /= 0
                 else bijabc == 0
                 where bijabc = bij simpleEC a b c

prop_li :: Double -> Double -> Property
prop_li a b = b > 0 ==> li simpleEC a b /= 0

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