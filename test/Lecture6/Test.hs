module Lecture6.Test (tests) where

import Test.HUnit hiding (cases)
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Lecture6.Fibonacci (streamToList, streamRepeat, nats, fibs2, fibs3)

streamRepeatProp :: Positive Int -> Bool
streamRepeatProp (Positive n) = (take n . streamToList $ streamRepeat 1) == (take n $ repeat 1)

natsProp :: Positive Int -> Bool
natsProp (Positive n) = (take n $ streamToList nats) == (take n $ iterate (+1) 0)

fibs3Prop :: Positive Int -> Bool
fibs3Prop (Positive n) = (take n fibs2) == (take n . streamToList $ fibs3)

tests = testGroup "Lecture6"
    [ testProperty "streamRepeat" streamRepeatProp
    , testProperty "nats" natsProp
    , testProperty "fibs3" fibs3Prop
    ]
