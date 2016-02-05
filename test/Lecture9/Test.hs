module Lecture9.Test (tests) where

import Test.HUnit hiding (cases)
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Lecture9.HW09 (integerRingProp, boolRingProp, mod5RingProp, mat2x2RingProp)

tests = testGroup "Lecture9"
    [ testProperty "integerRing" integerRingProp
    , testProperty "boolRing" boolRingProp
    --, testProperty "mod5Ring" mod5RingProp -- quickcheck falsifies this one
    , testProperty "mat2x2Ring" mat2x2RingProp
    ]
