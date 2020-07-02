import           UtilsTest
import           FEMTest

main :: IO ()
main = do
  putStrLn "\n*** Testing module Utils... ***"
  testUtils
  putStrLn "\n*** Testing module FEM... ***"
  testFEM
