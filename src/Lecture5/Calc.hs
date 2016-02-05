{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lecture5.Calc where

import qualified Lecture5.ExprT as ExprT
import Lecture5.Parser (parseExp)
import qualified Lecture5.StackVM as StackVM

import Data.Ord
import qualified Data.Map as M

class HasVars a where
  var :: String -> a

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance HasVars VarExprT where
  var = Var

data VarExprT
        = Lit Integer
        | Add VarExprT VarExprT
        | Mul VarExprT VarExprT
        | Var String
        deriving (Eq, Show)

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add mx my = (\m -> (+) <$> mx m <*> my m)
  mul mx my = (\m -> (*) <$> mx m <*> my m)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr StackVM.Program where
  lit n = [ StackVM.PushI n ]
  add xs ys = xs ++ ys ++ [ StackVM.Add ]
  mul xs ys = xs ++ ys ++ [ StackVM.Mul ]

newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr Mod7 where
  lit = Mod7 . (`rem` 7) . abs
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x * y)

newtype MinMax = MinMax Integer deriving (Show, Eq, Ord)

instance Expr MinMax where
  lit = MinMax
  add = (max)
  mul = (min)

instance Expr Bool where
  lit = (>=0)
  add = (||)
  mul = (&&)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr ExprT.ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT.ExprT -> ExprT.ExprT
reify = id

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

eval :: ExprT.ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add x y) = (eval x) + (eval y)
eval (ExprT.Mul x y) = (eval x) * (eval y)
