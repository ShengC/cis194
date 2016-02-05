{-# LANGUAGE TemplateHaskell #-}

module Lecture11.SpliceFunctionsApp where

import Language.Haskell.TH

import qualified Lecture11.SpliceFunctions as SF

eleven :: Integer
eleven = $( SF.add5 6 )

twelve :: Integer
twelve = $( SF.compileTimeAdd5 7 )

liftM4 :: $( SF.liftMType 4 )
liftM4 f = $( SF.liftMBody 4 )

liftM6 :: $( SF.liftMType 6 )
liftM6 f = $( SF.liftMBody' 6 )

liftM7 :: $( SF.liftMType' 7 )
liftM7 f = $( SF.liftMBody' 7 )
