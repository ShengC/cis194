module Lecture9.HW09 where

import Test.QuickCheck
import Control.Monad
import System.Random

import Lecture9.Ring
import Lecture9.BST

instance Arbitrary Mod5 where
  arbitrary = arbitrary >>= return . MkMod

instance Arbitrary Mat2x2 where
  arbitrary = MkMat <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (MkMat a b c d) = [MkMat x1 x2 y1 y2 | (x1, x2, y1, y2) <- shrink (a, b, c, d)]

associativeProp :: (Ring a, Eq a) => a -> a -> a -> Bool
associativeProp a b c = (a `add` b) `add` c == a `add` (b `add` c)

communicativeProp :: (Ring a, Eq a) => a -> a -> Bool
communicativeProp a b = a `add` b == b `add` a

additiveIdentityProp :: (Ring a, Eq a) => a -> Bool
additiveIdentityProp a = a `add` addId == a

additiveInverseProp :: (Ring a, Eq a) => a -> Bool
additiveInverseProp a = a `add` (addInv a) == addId

mulAssociativeProp :: (Ring a, Eq a) => a -> a -> a -> Bool
mulAssociativeProp a b c = (a `mul` b) `mul` c == a `mul` (b `mul` c)

mulIdentityProp :: (Ring a, Eq a) => a -> Bool
mulIdentityProp a = (a `mul` mulId) == a

leftDistributiveProp :: (Ring a, Eq a) => a -> a -> a -> Bool
leftDistributiveProp a b c = a `mul` (b `add` c) == (a `mul` b) `add` (a `mul` c)

rightDistributiveProp :: (Ring a, Eq a) => a -> a -> a -> Bool
rightDistributiveProp a b c = (b `add` c) `mul` a == (b `mul` a) `add` (c `mul` a)

ringProp :: (Ring a, Eq a) => a -> a -> a -> Property
ringProp a b c =
  property (associativeProp a b c) .&.
    property (communicativeProp a b) .&.
      property (additiveIdentityProp a) .&.
        property (mulAssociativeProp a b c) .&.
          property (mulIdentityProp a) .&.
            property (leftDistributiveProp a b c) .&.
              property (rightDistributiveProp a b c)

integerRingProp :: Integer -> Integer -> Integer -> Property
integerRingProp = ringProp

boolRingProp :: Bool -> Bool -> Bool -> Property
boolRingProp = ringProp

mod5RingProp :: Mod5 -> Mod5 -> Mod5 -> Property
mod5RingProp = ringProp

mat2x2RingProp :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Property
mat2x2RingProp = ringProp

isBSTBetween2 :: (Ord a) => Maybe a -> Maybe a -> BST a -> Bool
isBSTBetween2 _ _ Leaf = True
isBSTBetween2 m_lower m_upper (Node left x right)
  = isBSTBetween2 m_lower (Just x) left &&
    isBSTBetween2 (Just x) m_upper right &&
    case compare <$> m_lower <*> (Just x) of
      Just GT -> False
      _       -> True
    &&
    case compare <$> (Just x) <*> m_upper of
      Just GT -> False
      _       -> True

isBST2 :: (Ord a) => BST a -> Bool
isBST2 = isBSTBetween2 Nothing Nothing

genBST :: (Ord a, Random a) => a -> a -> Gen (BST a)
genBST lower upper =
  (arbitrary :: Gen Bool) >>= \b ->
    if b then return Leaf
         else choose(lower, upper) >>= \x ->
           Node <$> genBST lower x <*> pure x <*> genBST x upper

instance (Ord a, Random a, Arbitrary a) => Arbitrary (BST a) where
  arbitrary =
    do x <- arbitrary
       y <- arbitrary
       if (x < y) then genBST x y else genBST y x

