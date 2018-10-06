{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}

module Eval where

import AST

import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.Generics.Uniplate.Data
import qualified Data.Map as M
import Primitives
import RIO

eval :: AST -> Reader (Bindings SimpleAST) SimpleAST
eval = cata evalExpr

tryFind :: String -> Bindings SimpleAST -> SimpleAST
tryFind key m =
  case M.lookup key m of
    Just v -> v
    Nothing -> error $ "couldn't find symbol: " ++ key

evalExpr ::
     ASTF (Reader (Bindings SimpleAST) SimpleAST)
  -> Reader (Bindings SimpleAST) SimpleAST
evalExpr (Appl f arg) = do
  f' <- f
  case unfix f' of
    (SBuiltin builtin args) -> do
      runBuiltin builtin args <$> arg
    (SBinding name expr) -> do
      local (<> [(name, expr)]) arg
    (SFuncDef argName expr) -> do
      arg' <- arg
      return $ subBindings argName arg' expr
    _ -> error "got unknown thing in Appl"
evalExpr (List rs) = Fix . SList <$> sequenceA rs
evalExpr (Symbol name) = asks (fromMaybe (Fix $ SFuncArg name) . M.lookup name)
evalExpr (Str s) = return . Fix $ SStr s
evalExpr (Number n) = return . Fix $ SNumber n
evalExpr (Boolean b) = return . Fix $ SBoolean b
evalExpr (FuncDef argName expr) = sfuncDef argName <$> expr
evalExpr (Binding name expr) = Fix . SBinding name <$> expr

subBindings :: String -> SimpleAST -> SimpleAST -> SimpleAST
subBindings argName sub = transform go
  where
    go (unfix -> SFuncArg s)
      | s == argName = sub
    go x = x

-- nEvalExpr :: SimpleASTF NoBindingsAST -> NoBindingsAST
-- nEvalExpr (SFunc (AFunc f)) = Fix $ NFunc (AFunc (f . undefined))
-- nEvalExpr (SList rs) = Fix $ NList rs
-- nEvalExpr (SStr s) = Fix $ NStr s
-- nEvalExpr (SNumber n) = Fix $ NNumber n
-- nEvalExpr (SBoolean b) = Fix $ NBoolean b
-- nEvalExpr (SFuncArg s) = error $ "missed symbol substitution: " ++ s
-- nToS :: NoBindingsAST -> SimpleAST
-- nToS _ = error "nope"
-- nToS (unfix -> NList rs) = Fix $ SList rs
-- nToS (unfix -> NFunc (AFunc f)) = Fix $ SFunc (AFunc (_ . f . _))
-- nToS (unfix -> NStr s) = Fix $ SStr s
-- nToS (unfix -> NNumber s) = Fix $ SNumber s
-- nToS (unfix -> NBoolean s) = Fix $ SBoolean s
pEvalExpr :: NoBindingsASTF Prim -> Prim
pEvalExpr (NList rs) = PList rs
pEvalExpr (NFunc _) = error $ "unapplied func!"
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
