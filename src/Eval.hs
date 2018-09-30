{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}

module Eval where

import AST
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Foldable
import qualified Data.Map as M
import RIO

eval :: AST -> Reader Bindings Prim
eval = cata evalExpr

evalExpr :: ASTF (Reader Bindings Prim) -> Reader Bindings Prim
evalExpr (P p) = pure p
evalExpr (FuncDef argName expr) = do
  binds <- ask
  return . Func $ \p -> flip runReader (binds <> [(argName, p)]) expr
evalExpr (Appl f args) = applyArgs f args
evalExpr (Symbol s) = do
  mPrim <- asks (M.lookup s)
  case mPrim of
    Nothing -> error $ "Symbol " ++ s ++ " is not in scope!"
    Just v -> return v
evalExpr (List _) = error $ "List is unimplemented"
evalExpr (Bindings b) = do
  error "bind"
  -- rec binds <- local (<> binds) (traverse eval b)
  -- return (BindingsPrim binds)

applyArgs ::
     Reader Bindings Prim -> Reader Bindings Prim -> Reader Bindings Prim
applyArgs f arg = do
  f' <- f
  case f' of
    (Func func) -> func <$> arg
    (BindingsPrim b) -> local (<> b) arg
    x ->
      error $ "expected function or binding in application but got: " ++ show x
