module Lecture4.Test (tests) where

import Test.HUnit hiding (cases)
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Lecture4.Exercise (fun1', fun2', fun1, fun2, foldTree, isBalanced, xor, myFoldl)

fun1Test :: Assertion
fun1Test = ((6 - 2) * (4 - 2) * 1) @=? fun1' [6,5,4,3]

fun2Test :: Assertion
fun2Test = (10 + 16 + 8 + 4 + 2) @=? fun2' 10

foldlTest :: Assertion
foldlTest = foldl (flip (:)) [] [1.. 10] @=? myFoldl (flip (:)) [] [1..10]

fun1Prop :: [Integer] -> Bool
fun1Prop is = (fun1' is) == (fun1 is)

-- need to figure out how to limit the input
fun2Prop :: Integer -> Bool
fun2Prop i = (fun2' i) == (fun2 i)

treeProp :: String -> Bool
treeProp = isBalanced . foldTree

xorProp :: [Bool] -> Bool
xorProp xs = xor xs == (odd . length . filter id $ xs)

tests = testGroup "Lecture4"
  [ testCase "fun1'" fun1Test
  , testCase "fun2'" fun2Test
  , testCase "myFoldl" foldlTest
  --, testProperty "fun2" fun2Prop
  , testProperty "tree" treeProp
  , testProperty "xor" xorProp
  ]
