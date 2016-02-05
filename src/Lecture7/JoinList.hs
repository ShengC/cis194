module Lecture7.JoinList where

import Lecture7.Sized

import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Show, Eq)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
xl +++ yl = Append (tag xl <> tag yl) xl yl

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i - 1)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single _ a)
  | n == 0 = Just a
  | otherwise = Nothing
indexJ n (Append m left right)
  | n >= (getSize . size $ m)  && n < 0 = Nothing
  | otherwise =
      let s = getSize . size . tag $ left
      in  if s > n
             then indexJ n left
             else indexJ (n - s) right

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n single@(Single _ _)
  | n >= 1 =    Empty
  | otherwise = single
dropJ n append@(Append m left right)
  | n >= (getSize . size $ m) = Empty
  | n <= 0 =       append
  | otherwise =
      let s = getSize . size . tag $ left
      in  if s > n
             then dropJ n left +++ right
             else dropJ (n - s) right

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n single@(Single _ _)
  | n >= 1 = single
  | otherwise = Empty
takeJ n append@(Append m left right)
  | n >= (getSize . size $ m) = append
  | n <= 0       = Empty
  | otherwise =
      let s = getSize . size . tag $ left
      in  if s > n
             then takeJ n left
             else left +++ takeJ (n - s) right
