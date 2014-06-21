import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.Tasty.SmallCheck as SC

arith :: Integer -> Integer -> Property
arith x y = (x > 0) && (y > 0) ==> (x+y)^2 > x^2 + y^2

negation :: Integer -> Bool
negation x = abs (x^2) >= x

suite :: TestTree
suite = testGroup "Test Suite" [
    testGroup "Units"
      [ testCase "Equality" $ True @=? True
      , testCase "Assertion" $ assert $ (length [1,2,3]) == 3
      ],

    testGroup "QuickCheck tests"
      [ testProperty "Quickcheck test" arith
      ],

    testGroup "SmallCheck tests"
      [ SC.testProperty "Negation" negation
      ]
  ]

main :: IO ()
main = defaultMain suite
