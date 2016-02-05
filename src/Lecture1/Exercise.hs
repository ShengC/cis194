module Lecture1.Exercise where

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise =
      let y = x `div` 10
          z = x `rem` 10
      in  (toDigits y) ++ [z]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise =
      let y = x `div` 10
          z = x `rem` 10
      in  z : (toDigitsRev y)

-- could make use of `toDigitRev`
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  let doubleEven (x:xs) = (2*x):(doubleOdd xs)
      doubleEven [] = []
      doubleOdd (x:xs) = x:(doubleEven xs)
      doubleOdd [] = []
  in  if (length xs) `mod` 2 == 0
         then doubleEven xs
         else doubleOdd  xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum . toDigits $ x) + (sumDigits xs)

validate :: Integer -> Bool
validate = (\x -> x `mod` 10 == 0) . sumDigits . doubleEveryOther . toDigits

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 x y _ = [(x, y)]
hanoi n x y z = hanoi (n - 1) x z y ++ [(x, y)] ++ hanoi (n - 1) z y x
