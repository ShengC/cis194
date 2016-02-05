module Lecture11.Test (tests) where

import Test.HUnit hiding (cases)
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.Char as C

import qualified Lecture11.AParser as AParser
import Lecture11.SExpr (zeroOrMore, oneOrMore, ident, parseSExpr, Atom(..), SExpr(..))

zeroOrMoreTest :: Assertion
zeroOrMoreTest = AParser.runParser (zeroOrMore (AParser.satisfy C.isUpper)) "abcdeFGh" @=? Just ("", "abcdeFGh")

oneOrMoreTest :: Assertion
oneOrMoreTest = AParser.runParser (oneOrMore (AParser.satisfy C.isUpper)) "abcdeFGh" @=? Nothing

identTest :: Assertion
identTest = AParser.runParser ident "foobar baz" @=? Just ("foobar", " baz")

identTest2 :: Assertion
identTest2 = AParser.runParser ident "foo33fA" @=? Just ("foo33fA", "")

identTest3 :: Assertion
identTest3 = AParser.runParser ident "2bad" @=? Nothing

identTest4 :: Assertion
identTest4 = AParser.runParser ident "" @=? Nothing

parseSExprTest :: Assertion
parseSExprTest = AParser.runParser parseSExpr "5" @=? Just (A . N $ 5, "")

parseSExprTest2 :: Assertion
parseSExprTest2 = AParser.runParser parseSExpr "foo3" @=? Just (A . I $ "foo3", "")

parseSExprTest3 :: Assertion
parseSExprTest3 =
  let s = Comb [A . I $ "lots", A . I $ "of", Comb [A . I $ "spaces", A . I $ "in"], A . I $ "this", Comb [A . I $ "one"]]
  in  AParser.runParser parseSExpr "( lots of ( spaces in ) this ( one ) )" @=? Just (s, "")

parseSExprTest4 :: Assertion
parseSExprTest4 =
  let s = Comb [Comb [Comb [A . I $ "lambda", A . I $ "x", Comb [A . I $ "lambda", A . I $ "y", Comb [A . I $ "plus", A . I $ "x", A . I $ "y"]]], A . N $ 3], A . N $ 5]
  in  AParser.runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" @=? Just (s, "")

tests = testGroup "Lecture11"
    [ testCase "zeroOrMoreTest" zeroOrMoreTest
    , testCase "oneOrMoreTest" oneOrMoreTest
    , testGroup "identTest"
      [ testCase "1" identTest
      , testCase "2" identTest2
      , testCase "3" identTest3
      , testCase "4" identTest4
      ]
    , testGroup "parseSExprTest"
      [ testCase "1" parseSExprTest
      , testCase "2" parseSExprTest2
      , testCase "3" parseSExprTest3
      , testCase "4" parseSExprTest4
      ]
    ]
