{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Data.List
import Data.Typeable
import QuickSpec hiding (arith, bools, lists)
import Test.QuickCheck.Arbitrary

type Var k a = (Typeable a, Arbitrary a, CoArbitrary a, k a)

listCons :: forall a. Var Ord a => a -> Sig
listCons a =
  background
    [ "[]" `fun0` ([] :: [a]),
      ":" `fun2` ((:) :: a -> [a] -> [a])
    ]

lists :: forall a. Var Ord a => a -> [Sig]
lists a =
  [ -- Names to print arbitrary variables
    funs',
    funvars',
    vars',
    -- Ambient definitions
    listCons a,
    -- Expressions to deduce properties of
    "sort" `fun1` (sort :: [a] -> [a]),
    "map" `fun2` (map :: (a -> a) -> [a] -> [a]),
    "id" `fun1` (id :: [a] -> [a]),
    "reverse" `fun1` (reverse :: [a] -> [a]),
    "minimum" `fun1` (minimum :: [a] -> a),
    "length" `fun1` (length :: [a] -> Int),
    "++" `fun2` ((++) :: [a] -> [a] -> [a])
  ]
  where
    funs' = funs (undefined :: a)
    funvars' = vars ["f", "g", "h"] (undefined :: a -> a)
    vars' = ["xs", "ys", "zs"] `vars` (undefined :: [a])

tvar :: A
tvar = undefined

main :: IO ()
main = quickSpec (lists tvar)
