module Lecture5.Test (tests) where

import Test.HUnit hiding (cases)
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.Map as M

import qualified Lecture5.ExprT as ExprT
import Lecture5.Parser (parseExp)
import Lecture5.Calc (eval, evalStr, Expr(..), MinMax(..), Mod7(..), VarExprT(..), HasVars(..))

evalTest :: Assertion
evalTest = eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)) @=? 20

evalStrTest1 :: Assertion
evalStrTest1 = evalStr "(2+3)*4" @=? Just 20

evalStrTest2 :: Assertion
evalStrTest2 = evalStr "2+3*4" @=? Just 14

evalStrTest3 :: Assertion
evalStrTest3 = evalStr "2+3*" @=? Nothing

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Assertion
testInteger = (testExp :: Maybe Integer) @=? Just (3 * (-4) + 5)

testBool :: Assertion
testBool = (testExp :: Maybe Bool) @=? Just ((True || False) && True)

testMM :: Assertion
testMM = (testExp :: Maybe MinMax) @=? Just (MinMax (( 3 `min` (-4) ) `max` 5))

testSat :: Assertion
testSat = (testExp :: Maybe Mod7) @=? Just (Mod7 ( (((3 `rem` 7) * (4 `rem` 7)) `rem` 7 + (5 `rem` 7)) `rem` 7 ))

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

testWithVars1 :: Assertion
testWithVars1 = (withVars [("x", 6)] $ add (lit 3) (var "x")) @=? Just 9

testWithVars2 :: Assertion
testWithVars2 = (withVars [("x", 6)] $ add (lit 3) (var "y")) @=? Nothing

testWithVars3 :: Assertion
testWithVars3 = (withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))) @=? Just 54

tests = testGroup "Lecture5"
    [ testCase "eval" evalTest
    , testGroup "evalStr"
      [ testCase "1" evalStrTest1
      , testCase "2" evalStrTest2
      , testCase "3" evalStrTest3
      ]
    , testGroup "testExp"
      [ testCase "Integer" testInteger
      , testCase "Bool" testBool
      , testCase "MinMax" testMM
      , testCase "Mod7" testSat
      ]
    , testGroup "withVar"
      [ testCase "1" testWithVars1
      , testCase "2" testWithVars2
      , testCase "3" testWithVars3
      ]
    ]

