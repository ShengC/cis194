module Lecture3.Test (tests) where

import Test.HUnit hiding (cases)
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Lecture3.Golf (skips, localMaxima, histogram)

skipsTest :: Assertion
skipsTest = ["ABCD", "BD", "C", "D"] @=? skips "ABCD"

localMaximaTest :: Assertion
localMaximaTest = [9, 6] @=? localMaxima [2, 9, 5, 6, 1]

histogramTest :: Assertion
histogramTest = "   * *    \n==========\n0123456789\n" @=? histogram [3,5]

tests = testGroup "Lecture3"
    [ testCase "skips" skipsTest
    , testCase "localMaxima" localMaximaTest
    , testCase "histogram" histogramTest
    ]
