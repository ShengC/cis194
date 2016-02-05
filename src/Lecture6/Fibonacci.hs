{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Lecture6.Fibonacci where

import Data.List

data Matrix =
  Matrix {
    a :: Integer,
    b :: Integer,
    c :: Integer,
    d :: Integer
  }

instance Num Matrix where
  (*) (Matrix x1 x2 x3 x4) (Matrix y1 y2 y3 y4) =
    Matrix (x1 * y1 + x2 * y3) (x1 * y2 + x2 * y4) (x3 * y1 + x4 * y3) (x3 * y2 + x4 * y4)

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = a $ (Matrix 1 1 1 0) ^ n

data Stream a = Cons a (Stream a)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger x = Cons x $ streamRepeat 0
  negate (Cons x xs) = Cons (-x) $ negate xs
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
  (*) (Cons x xs) (Cons y ys) = Cons (x * y) (streamMap (*x) ys + (xs * (Cons y ys)))

instance Fractional (Stream Integer) where
  (/) (Cons x xs) (Cons y ys) =
    let q = Cons (x `div` y) $ streamMap (`div` y) (xs - q * ys)
    in  q

ruler :: Stream Integer
ruler = foldr1 interleaveStreams $ map streamRepeat [0..]

-- implementation of this function is interesting:
-- we have to make sure the second argument `b` is non strict
-- otherwise in case of interleaving infinite list of streams, it would actually run forever
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) b =
  Cons a (case b of Cons b bs -> Cons b (interleaveStreams as bs))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a as) = a : streamToList as

fibs2 :: [Integer]
fibs2 = map f [0..]
  where f 0 = 0
        f 1 = 1
        f n = fibs2 !! (n - 1) + fibs2 !! (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
