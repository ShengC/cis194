module Lecture7.Note where

import Data.Monoid

newtype ANDBool = ANDBool { getANDBool :: Bool } deriving (Eq, Show)
newtype ORBool = ORBool { getORBool :: Bool } deriving (Eq, Show)

instance Monoid ANDBool where
  mempty = ANDBool True
  mappend (ANDBool x) (ANDBool y)= ANDBool (x && y)

instance Monoid ORBool where
  mempty = ORBool False
  mappend (ORBool x) (ORBool y)= ORBool (x || y)

{--
instance Monoid b => Monoid (a -> b) where
  mempty _ = mempty
  mappend f g x = f x <> g x
--}
