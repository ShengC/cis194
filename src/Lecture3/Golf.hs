module Lecture3.Golf where

import Data.List (uncons, unlines)
import Data.Char (intToDigit)

histogram :: [Integer] -> String
histogram xs =
  let c = counts xs
      d = dist c []
      t = d ++ axis
  in  unlines t

axis :: [String]
axis = (replicate 10 '=') : (map intToDigit [0..9]) : []

dist :: [Int] -> [String] -> [String]
dist xs ls =
  if sum xs == 0
     then ls
     else let l = map (\x -> if (x > 0) then '*' else ' ') xs
              ys = map (\x -> if (x > 0) then x - 1 else 0) xs
          in  dist ys (l:ls)

counts :: [Integer] -> [Int]
counts xs = map (\y -> length . filter (y==) $ xs) [0..9]

localMaxima :: [Integer] -> [Integer]
localMaxima xs =
  case uncons xs of
    Just (x, y:z:zs) ->
      if (x <= y) && (z <= y)
         then y : (localMaxima (z:zs))
         else (localMaxima (y:z:zs))
    _                -> []

skips :: [a] -> [[a]]
skips xs = map ((flip every) xs) [1..(length xs)]

every :: Int -> [a] -> [a]
every n xs = every' n n xs

every' :: Int -> Int -> [a] -> [a]
every' n m xs =
  case uncons xs of
    Just (y, ys) ->
      if m == 1
         then y : (every' n n ys)
         else every' n (m - 1) ys
    Nothing      -> []
