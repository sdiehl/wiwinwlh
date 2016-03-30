import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Silently

test :: Int -> IO ()
test n = print (n * n)

testCapture n = do
  (stdout, result) <- capture (test n)
  assert (stdout == show (n*n) ++ "\n")

suite :: TestTree
suite = testGroup "Test Suite" [
    testGroup "Units"
      [ testCase "Equality" $ testCapture 10
      ]
  ]

main :: IO ()
main = defaultMain suite
