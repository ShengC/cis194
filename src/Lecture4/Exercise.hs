module Lecture4.Exercise where

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let xs = [ (i + j + 2 * i * j) | i <- [1..n], j <- [1..n], (i + j + 2 * i * j) <= n ]
  in  map ((+1) . (2*)) . filter (\x -> not (x `elem` xs)) $ [1..n]

myFoldl :: (a -> b-> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\b g -> (\a -> g (f a b))) id xs base

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> (f a) : bs) []

xor :: [Bool] -> Bool
xor =
  let step a False   = a
      step True True = False
      step False True = True
  in  foldl step False

foldTree :: [a] -> Tree a
foldTree =
  let step :: a -> Tree a -> Tree a
      step a xs =
        case xs of
          Leaf -> Node 0 Leaf a Leaf
          Node _ Leaf b Leaf   -> Node 1 (Node 0 Leaf a Leaf) b Leaf
          Node n Leaf b right  -> Node n (Node 0 Leaf a Leaf) b right
          Node n left b Leaf   -> Node n left b (Node 0 Leaf a Leaf)
          Node n left@(Node x _ _ _) b right@(Node y _ _ _) ->
            if x <= y
              then
                case step a left of
                  node@(Node z _ _ _) ->
                    if z <= y
                       then Node n node b right
                       else Node (z+1) node b right
              else
                case step a right of
                  node@(Node z _ _ _) ->
                    if z <= x
                       then Node n left b node
                       else Node (z+1) left b node
  in foldr step Leaf

height :: Tree a -> Integer
height Leaf = 0
height (Node n _ _ _) = n

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ left _ right) =
  (abs (height left - height right) <= 1) && isBalanced left && isBalanced right

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs) | even x    = (x - 2) * fun1 xs
            | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' =  foldr (*) 1 . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' x =
  let y = if even x then x else (3 * x + 1)
      step 1 = 0
      step 2 = 1
      step n =
        let m = n `div` 2
        in  if even m then m else (3 * m + 1)
  in sum . takeWhile (>1) . iterate step $ y

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)


