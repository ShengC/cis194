{-# LANGUAGE TemplateHaskell #-}

module Lecture11.SpliceFunctions where

import Control.Monad ( replicateM )
import Data.Maybe ( maybeToList )

import Language.Haskell.TH

add5 :: Integer -> Q Exp
add5 n = return (AppE (AppE (VarE (mkName "+")) (LitE (IntegerL n))) (LitE (IntegerL 5)))

compileTimeAdd5 :: Integer -> Q Exp
compileTimeAdd5 n = return (LitE (IntegerL (n + 5)))

listOfAs :: Q Exp
listOfAs = return (ListE (map VarE [ mkName ('a' : show n) | n <- [1..1000] ]))

liftMBody :: Int -> Q Exp
liftMBody n = let m_names = take n [ mkName ('m' : [x]) | x <- ['a'..] ]
                  names   = take n [ mkName [x]         | x <- ['a'..] ]
                  binds   = zipWith mk_bind m_names names
                  ret     = NoBindS (AppE (VarE (mkName "return"))
                                          (mk_apps (VarE (mkName "f"))
                                                   (map VarE names)))

                  mk_bind :: Name -> Name -> Stmt
                  mk_bind m_name name = BindS (VarP name) (VarE m_name)

                  mk_apps :: Exp -> [Exp] -> Exp
                  mk_apps = foldl AppE
              in
              return $ LamE (map VarP m_names) (DoE (binds ++ [ ret ]))

returnName :: Name
returnName = 'return

liftMBody' n = do m_names <- replicateM n (newName "m")
                  names   <- replicateM n (newName "a")
                  let binds = zipWith mk_bind m_names names
                      ret   = NoBindS (AppE (VarE 'return)
                                      (mk_apps (VarE (mkName "f"))
                                               (map VarE names)))

                      mk_bind :: Name -> Name -> Stmt
                      mk_bind m_name name = BindS (VarP name) (VarE m_name)

                      mk_apps :: Exp -> [Exp] -> Exp
                      mk_apps = foldl AppE
                  return $ LamE (map VarP m_names) (DoE (binds ++ [ret]))

liftMType :: Int -> Q Type
liftMType n = let names = take n [ mkName [x] | x <- ['a'..] ]
                  types = map VarT names
                  m     = mkName "m"
                  res   = mkName "r"
                  resty = VarT res

                  -- make nested arrows
                  mk_arrs :: [Type] -> Type -> Type
                  mk_arrs []     result = result
                  mk_arrs (x:xs) result = AppT (AppT ArrowT x) (mk_arrs xs result)

                  -- apply "m" to a type
                  app_m :: Type -> Type
                  app_m = AppT (VarT m)
              in
              return $ ForallT (PlainTV m : PlainTV res :
                          map PlainTV names)
                          [AppT (ConT ''Monad) (VarT m)]
                          --[classP ''Monad [varT m]]
                          (mk_arrs (mk_arrs types resty : map app_m types) (app_m resty))

liftMType' :: Int -> Q Type
liftMType' n = do
  names <- replicateM n (newName "a")
  m     <- newName "m"
  res   <- newName "r"

  let types = map varT names
      mty   = varT m
      resty = varT res

      mk_arrs :: [Q Type] -> Q Type -> Q Type
      mk_arrs [] result = result
      mk_arrs (x:xs) result = [t| $x -> $(mk_arrs xs result) |]

      app_m :: Q Type -> Q Type
      app_m ty = [t| $mty $ty |]

  forallT (map PlainTV (m : res : names)) (cxt [])
          [t| Monad $mty -> $(mk_arrs types resty) ->
                            $(mk_arrs (map app_m types) (app_m resty) ) |]
