{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Eval where

import AST
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Data.Functor.Foldable
import RIO

eval :: AST -> Reader Bindings Prim
eval = cata evalExpr

evalExpr :: ASTF (Reader Bindings Prim) -> Reader Bindings Prim
evalExpr (P p) = pure p
evalExpr (FuncDef argName expr) = do
  binds <- ask
  return . Func $ \p -> flip runReader (binds <> [(argName, p)]) expr
evalExpr (Appl f args) = do
  f' <- f
  args' <- sequenceA args
  return $ foldr' apply f' args'
  where
    apply :: Prim -> Prim -> Prim
    apply (Func func) a = func a
    apply x _ = error $ "expected function in application but got: " ++ show x
-- evalExpr (List _) = undefined
-- evalExpr (Builtin _) = undefined
-- evalExpr (Symbol _) = undefined
-- evalExpr (Bindings _) = undefined
-- evalExpr (BindingsPrim _) = undefined
