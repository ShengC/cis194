{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Lecture7.Scrabble where

import Data.Char (toUpper)
import Data.Monoid
import Lecture7.JoinList (JoinList(..), jlToList, indexJ, takeJ, dropJ, (+++), tag)
import Lecture7.Sized (Size(..))
import Lecture7.Buffer

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

instance Buffer (JoinList (Score, Size) String) where
  toString = show . jlToList
  fromString s = Single (scoreString s, 1) s
  line = indexJ
  replaceLine n s b =
    case line n b of
      Nothing -> b
      Just _  ->
        takeJ n b +++ fromString s +++ dropJ (n + 1) b
  numLines b =
    case (snd . tag $ b) of Size n -> n
  value b =
    case (fst . tag $ b) of Score c -> c

score :: Char -> Score
score char =
  case toUpper char of
    'A' -> Score 1
    'B' -> Score 3
    'C' -> Score 3
    'D' -> Score 2
    'E' -> Score 1
    'F' -> Score 4
    'G' -> Score 2
    'H' -> Score 4
    'I' -> Score 1
    'J' -> Score 8
    'K' -> Score 5
    'L' -> Score 1
    'M' -> Score 3
    'N' -> Score 1
    'O' -> Score 1
    'P' -> Score 3
    'Q' -> Score 10
    'R' -> Score 1
    'S' -> Score 1
    'T' -> Score 1
    'U' -> Score 1
    'V' -> Score 4
    'W' -> Score 4
    'X' -> Score 8
    'Y' -> Score 4
    'Z' -> Score 10
    _   -> mempty

scoreString :: String -> Score
scoreString = foldl (<>) mempty . map score

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
