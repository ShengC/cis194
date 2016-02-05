module Lecture8.Party where

import Lecture8.Employee
import Data.Monoid
import Data.Tree
import Data.List (sort)

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL emps fun) = GL (employee:emps) (empFun employee + fun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL xs xf) (GL ys yf) = GL (xs ++ ys) (xf + yf)

-- since Ord instance of GuestList is already defined in Employee.hs
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) (map (treeFold f) (subForest t))

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e xs =
  let first = mconcat . map (\(x, y) -> x `moreFun` y) $ xs
      second = glCons e (mconcat . map fst $ xs)
  in  (first, second)

maxFun :: Tree Employee -> GuestList
maxFun tree =
  case treeFold nextLevel tree of
    (x, y) -> x `moreFun` y

company :: IO (Tree Employee)
company =
  do path <- getLine
     content <- readFile path
     return ((read content) :: (Tree Employee))

printFun :: Fun -> IO ()
printFun fun = putStrLn $ "Total fun: " ++ (show fun)

printEmployees:: [Employee] -> IO ()
printEmployees = mapM_ putStrLn . sort . map empName

printGl :: GuestList -> IO ()
printGl (GL employees fun) =
  do printFun fun
     printEmployees employees

main =
  do putStrLn "enter file path of company:"
     tree <- company
     gl <- return (maxFun tree)
     printGl gl
