module Lecture7.Test (tests) where

import Test.HUnit hiding (cases)
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Lecture7.Sized (Size(..))
import Lecture7.JoinList (JoinList(..), indexJ, jlToList, (!!?), dropJ, takeJ, (+++))
import Lecture7.Scrabble (Score(..), scoreLine)

joinListSample :: JoinList Size Char
joinListSample =
  Append
    (Size 4)
    (Append
      (Size 3)
      (Single (Size 1) 'y')
      (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a')))
    (Single (Size 1) 'h')

indexJProp :: Int -> Bool
indexJProp n = (indexJ n joinListSample) == (jlToList joinListSample !!? n)

dropJProp :: Int -> Bool
dropJProp n = (jlToList (dropJ n joinListSample)) == drop n (jlToList joinListSample)

takeJProp :: Int -> Bool
takeJProp n = (jlToList (takeJ n joinListSample)) == take n (jlToList joinListSample)

scoreLineTest :: Assertion
scoreLineTest = scoreLine "yay " +++ scoreLine "haskell!"  @=? Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")

tests = testGroup "Lecture7"
    [ testCase "scoreLine" scoreLineTest
    , testProperty "indexJ" indexJProp
    , testProperty "dropJ" dropJProp
    , testProperty "takeJ" takeJProp
    ]
