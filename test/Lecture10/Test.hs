module Lecture10.Test (tests) where

import Test.HUnit hiding (cases)
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Lecture10.AParser (abParser, abParser_, Parser(..), intPair, intOrUppercase)

abParserTest :: Assertion
abParserTest = runParser abParser "abcdef" @=? Just (('a', 'b'), "cdef")

abParserTest2 :: Assertion
abParserTest2 = runParser abParser "aedcbf" @=? Nothing

abParser_Test :: Assertion
abParser_Test = runParser abParser_ "abcdef" @=? Just ((), "cdef")

abParser_Test2 :: Assertion
abParser_Test2 = runParser abParser_ "aedcbf" @=? Nothing

intPairTest :: Assertion
intPairTest = runParser intPair "12 34" @=? Just ([12, 34], "")

intOrUppercaseTest :: Assertion
intOrUppercaseTest = runParser intOrUppercase "342abcd" @=? Just ((), "abcd")

intOrUppercaseTest2 :: Assertion
intOrUppercaseTest2 = runParser intOrUppercase "XYZ" @=? Just ((), "YZ")

intOrUppercaseTest3 :: Assertion
intOrUppercaseTest3 = runParser intOrUppercase "foo" @=? Nothing

tests = testGroup "Lecture10"
    [ testGroup "abParser"
      [ testCase "1" abParserTest
      , testCase "2" abParserTest2
      ]
    , testGroup "abParser_"
      [ testCase "1" abParser_Test
      , testCase "2" abParser_Test
      ]
    , testCase "intPair" intPairTest
    , testGroup "intOrUppercase"
      [ testCase "1" intOrUppercaseTest
      , testCase "2" intOrUppercaseTest2
      , testCase "3" intOrUppercaseTest3
      ]
    ]
