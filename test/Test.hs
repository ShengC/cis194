module Main where

import Test.Framework

import qualified Lecture3.Test
import qualified Lecture4.Test
import qualified Lecture5.Test
import qualified Lecture6.Test
import qualified Lecture7.Test
import qualified Lecture8.Test
import qualified Lecture9.Test
import qualified Lecture10.Test
import qualified Lecture11.Test

main :: IO ()
main = defaultMain
  [ Lecture3.Test.tests
  , Lecture4.Test.tests
  , Lecture5.Test.tests
  , Lecture6.Test.tests
  , Lecture7.Test.tests
  , Lecture8.Test.tests
  , Lecture9.Test.tests
  , Lecture10.Test.tests
  , Lecture11.Test.tests
  ]
