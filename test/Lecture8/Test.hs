module Lecture8.Test (tests) where

import Test.HUnit hiding (cases)
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Lecture8.Party (maxFun)
import Lecture8.Employee (testCompany, testCompany2)
import Lecture8.HW08 (stringFitsFormat)

testCompanyTest :: Assertion
testCompanyTest = assertBool "maxFun list cannot be empty" (not (maxFun testCompany == mempty ))

stringFitsFormatTest :: Assertion
stringFitsFormatTest = stringFitsFormat "3aaa2aa" @=? True

stringFitsFormatTest2 :: Assertion
stringFitsFormatTest2 = stringFitsFormat "9aaaaaaaaa" @=? True

stringFitsFormatTest3 :: Assertion
stringFitsFormatTest3 = stringFitsFormat "0" @=? True

stringFitsFormatTest4 :: Assertion
stringFitsFormatTest4 = stringFitsFormat "001a" @=? True

stringFitsFormatTest5 :: Assertion
stringFitsFormatTest5 = stringFitsFormat "2aa2aa" @=? True

stringFitsFormatTest6 :: Assertion
stringFitsFormatTest6 = stringFitsFormat "3aaa2a" @=? False

stringFitsFormatTest7 :: Assertion
stringFitsFormatTest7 = stringFitsFormat "10aaaaaaaaaa" @=? False

stringFitsFormatTest8 :: Assertion
stringFitsFormatTest8 = stringFitsFormat "1" @=? False

stringFitsFormatTest9 :: Assertion
stringFitsFormatTest9 = stringFitsFormat "100a" @=? False

stringFitsFormatTest10 :: Assertion
stringFitsFormatTest10 = stringFitsFormat "2bb2bb" @=? False

tests = testGroup "Lecture8"
  [ testCase "testCompanyTest" testCompanyTest
  , testGroup "stringFitsFormatTest"
    [ testCase "1" stringFitsFormatTest
    , testCase "2" stringFitsFormatTest2
    , testCase "3" stringFitsFormatTest3
    , testCase "4" stringFitsFormatTest4
    , testCase "5" stringFitsFormatTest5
    , testCase "6" stringFitsFormatTest6
    , testCase "7" stringFitsFormatTest7
    , testCase "8" stringFitsFormatTest8
    , testCase "9" stringFitsFormatTest9
    , testCase "10" stringFitsFormatTest10
    ]
  ]
