module Lecture7.JoinListBufEditor where

import Lecture7.Editor

import Lecture7.Buffer
import Lecture7.Sized
import Lecture7.Scrabble
import Lecture7.JoinList ((+++), JoinList(..))
import Data.Monoid

toTree :: [String] -> JoinList (Score, Size) String
toTree = foldl (+++) Empty . map fromString

main = runEditor editor $ toTree
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
