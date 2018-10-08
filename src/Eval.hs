{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}

module Eval where

import AST

import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.Generics.Uniplate.Data hiding (para)
import qualified Data.Map as M
import Primitives
import RIO

eval :: AST -> Reader (Bindings SimpleAST) NoBindingsAST
eval = fmap (cata nEvalExpr) . cata evalExpr

rebind :: AST -> AST
rebind = rewrite go
  where
    go :: AST -> Maybe AST
    go (unfix -> Binding name expr) =
      Just . Fix . Binding name $ rewrite (sub name expr) expr
    go _ = Nothing
    sub :: String -> AST -> AST -> Maybe AST
    sub name expr (unfix -> Symbol s)
      | s == name = Just $ rebind expr
    sub _ expr (unfix -> Rec) = Just $ rebind expr
    sub _ _ expr = Nothing

tryFind :: String -> Bindings SimpleAST -> SimpleAST
tryFind key m =
  case M.lookup key m of
    Just v -> v
    Nothing ->
      case M.lookup key primitives of
        Just v -> v
        Nothing -> Fix (SFuncArg key)

evalExpr ::
     ASTF (Reader (Bindings SimpleAST) SimpleAST)
  -> Reader (Bindings SimpleAST) SimpleAST
evalExpr (Appl f arg) = do
  f' <- f
  case unfix f' of
    (SBuiltin builtin args) -> do
      arg' <- arg
      return . Fix . SBuiltin builtin $ (args ++ [arg'])
    (SBinding name expr) -> local (<> [(name, expr)]) arg
    SRec -> error "y u SRec?"
    (SFuncDef argName expr) -> do
      arg' <- arg
      return $ subBindings argName arg' expr
    x -> error $ "got unknown thing in Appl: " ++ show x
evalExpr (List rs) = Fix . SList <$> sequenceA rs
evalExpr (Symbol name) = asks (tryFind name)
evalExpr (Str s) = return . Fix $ SStr s
evalExpr (Number n) = return . Fix $ SNumber n
evalExpr (Boolean b) = return . Fix $ SBoolean b
evalExpr (FuncDef argName expr) = do
  expr' <- expr
  return (Fix . SFuncDef argName $ expr')
evalExpr (Binding name expr) = Fix . SBinding name <$> expr

subBindings :: String -> SimpleAST -> SimpleAST -> SimpleAST
subBindings argName sub = transform go
  where
    go (unfix -> SFuncArg s)
      | s == argName = sub
    go x = x

nEvalExpr :: SimpleASTF NoBindingsAST -> NoBindingsAST
nEvalExpr (SList rs) = Fix $ NList rs
nEvalExpr (SStr s) = Fix $ NStr s
nEvalExpr (SNumber n) = Fix $ NNumber n
nEvalExpr (SBoolean b) = Fix $ NBoolean b
nEvalExpr (SFuncArg s) = error $ "missed symbol substitution: " ++ s
nEvalExpr (SFuncDef s _) = error $ "missed funcdef substitution: " ++ s
nEvalExpr (SBinding _ _) = error $ "missed bindings"
nEvalExpr (SBuiltin name args) = runBuiltin name args

backPort :: NoBindingsAST -> SimpleAST
backPort = cata go
  where
    go (NList rs) = Fix $ SList rs
    go (NStr s) = Fix $ SStr s
    go (NNumber s) = Fix $ SNumber s
    go (NBoolean s) = Fix $ SBoolean s

-- nToS :: NoBindingsAST -> SimpleAST
-- nToS _ = error "nope"
-- nToS (unfix -> NList rs) = Fix $ SList rs
-- nToS (unfix -> NFunc (AFunc f)) = Fix $ SFunc (AFunc (_ . f . _))
-- nToS (unfix -> NStr s) = Fix $ SStr s
-- nToS (unfix -> NNumber s) = Fix $ SNumber s
-- nToS (unfix -> NBoolean s) = Fix $ SBoolean s
pEvalExpr :: NoBindingsASTF Prim -> Prim
pEvalExpr (NList rs) = PList rs
pEvalExpr (NStr s) = PStr s
pEvalExpr (NNumber s) = PNumber s
pEvalExpr (NBoolean s) = PBoolean s
-- These last two will be thrown away anyways
-- sEvalExpr (SFunc _) = error "can't simplify func!"
-- sEvalExpr (SBinding _ bind) = error "binding!!!" -- sEvalExpr :: SimpleASTF Prim -> Prim
-- sEvalExpr (SAppl (SFunc (AFunc f)) arg) = f arg
-- sEvalExpr (SList rs) = PList rs
-- sEvalExpr (SStr s) = PStr s
-- sEvalExpr (SNumber n) = PNumber n
-- sEvalExpr (SBoolean b) = PBoolean b
-- sEvalExpr (SSymbol s) = error $ "missed symbol substitution: " ++ s
-- -- These last two will be thrown away anyways
-- sEvalExpr (SFunc _) = error "can't simplify func!"
-- sEvalExpr (SBinding _ bind) = error "binding!!!"
