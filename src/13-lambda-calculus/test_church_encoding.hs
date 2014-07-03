#!/usr/bin/env runhaskell

import ChurchEncoding

import Prelude hiding (not, succ, pred, fst, snd, tail, head, null, maybe, fmap)

import Test.Tasty
import Test.Tasty.HUnit

maybeAppendF  s = just (s ++ "x")
maybeNothingF _ = nothing

maybeTests = testGroup "maybe"
    [ testCase "nothing" $ maybe "default" id nothing @?= "default"
    , testCase "just"    $ maybe "default" id (just "just") @?= "just"

    , testCase "join nothing"        $ maybe "default" id (join nothing) @?= "default"
    , testCase "join $ just nothing" $ maybe "default" id (join $ just nothing) @?= "default"
    , testCase "join $ just just"    $ maybe "default" id (join $ just $ just "just") @?= "just"

    , testCase "fmap nothing" $ maybe "default" id (fmap (++ "x") nothing) @?= "default"
    , testCase "fmap just"    $ maybe "default" id (fmap (++ "x") (just "x")) @?= "xx"

    , testCase "bind nothing nothing" $ (maybe "default" id $ bind nothing maybeNothingF) @?= "default"
    , testCase "bind nothing f"       $ (maybe "default" id $ bind nothing maybeAppendF) @?= "default"
    , testCase "bind just nothing"    $ (maybe "default" id $ bind (just "x") maybeNothingF) @?= "default"
    , testCase "bind just f"          $ (maybe "default" id $ bind (just "x") maybeAppendF) @?= "xx"

    ]

list1 = cons "0" nil

listTests = testGroup "list"
    [ testCase "null nil"    $ (unbool $ null nil) @?= True
    , testCase "null cons"   $ (unbool $ null list1) @?= False
    , testCase "head nil"    $ (maybe "default" id $ head nil) @?= "default"
    , testCase "tail nil"    $ (maybe "default" id $ tail nil) @?= "default"
    , testCase "head cons"   $ (maybe "default" id $ head list1) @?= "0"
    , testCase "tail cons"   $ (unbool $ maybe false null $ tail list1) @?= True

    , testCase "index zero"  $ (maybe "default" id $ index list1 zero ) @?= "0"
    , testCase "index one"   $ (maybe "default" id $ index list1 one  ) @?= "default"

    ]

main = defaultMain $ testGroup "Tests" [ maybeTests, listTests ]
