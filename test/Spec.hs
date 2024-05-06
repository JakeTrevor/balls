import Test.Tasty (TestTree, defaultMain, testGroup)
import VectorTests (vectorTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [vectorTests]