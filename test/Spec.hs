import           Test.QuickCheck
import           UtilsTest
import           FEMTest
import           ParserTest

main :: IO ()
main = do
  putStrLn "\n*** Testing module Utils... ***"
  testUtils
  putStrLn "\n*** Testing module Parser... ***"
  testParser
  putStrLn "\n*** Testing module FEM... ***"
  testFEM
