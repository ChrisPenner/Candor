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

eval :: AST -> NoBindingsAST
eval = (cata nEvalExpr) . cata evalExpr

tryFind :: String -> Bindings SimpleAST -> SimpleAST
tryFind key m =
  case M.lookup key m of
    Just v -> v
    Nothing ->
      case M.lookup key primitives of
        Just v -> v
        Nothing -> Fix (SFuncArg key)

rewriteBinds :: String -> SimpleAST -> SimpleAST -> SimpleAST
rewriteBinds name val expr = rewrite go expr
  where
    go (unfix -> SFuncArg s)
      | s == name = Just val
    go _ = Nothing

evalExpr :: ASTF SimpleAST -> SimpleAST
evalExpr (Appl (unfix -> SFuncDef argName expr) arg) =
  rewriteBinds argName arg expr
evalExpr (Appl (unfix -> SBuiltin builtin args) arg) =
  Fix . SBuiltin builtin $ (args ++ [arg])
evalExpr (Appl (unfix -> SFuncArg s) arg) = Fix $ SAppl (Fix $ SFuncArg s) arg
evalExpr (Appl x arg) = Fix $ SAppl x arg
  -- case unfix f' of
  --   (SBuiltin builtin args) -> do
  --     return . Fix . SBuiltin builtin $ (args ++ [arg'])
  --   SRec -> error "y u SRec?"
  --   (SList rs) -> error "y u SList?"
  --   (SStr s) -> error "y u SStr?"
  --   (SNumber n) -> error "y u SNumber?"
  --   (SBoolean b) -> error "y u SBoolean?"
  --   (SFuncArg s) -> flip applyFunc arg' <$> asks (tryFind s)
    -- _ -> error $ "got unknown thing in Appl"
evalExpr (List rs) = Fix . SList $ rs
evalExpr (Symbol name) = Fix $ SFuncArg name
evalExpr (Str s) = Fix $ SStr s
evalExpr (Number n) = Fix $ SNumber n
evalExpr (Boolean b) = Fix $ SBoolean b
evalExpr (FuncDef argName expr) = Fix $ SFuncDef argName expr

-- applyFunc :: SimpleAST -> SimpleAST -> Reader (Bindings SimpleAST) SimpleAST
-- applyFunc (unfix -> SApplFunc func) arg = return $ func arg
-- applyFunc (unfix -> SBuiltin builtin args) arg =
--   return . Fix . SBuiltin builtin $ (args ++ [arg])
-- applyFunc (unfix -> SFuncArg s) arg = do
--   expr <- asks (tryFind s)
--   applyFunc expr arg
subBindings :: String -> SimpleAST -> SimpleAST -> SimpleAST
subBindings argName sub expr = cata go expr
  where
    go (SFuncArg s)
      | s == argName = sub
    go x = Fix x

nEvalExpr :: SimpleASTF NoBindingsAST -> NoBindingsAST
nEvalExpr (NAppl f arg) = Fix $ NList rs

evalExpr (SAppl (unfix -> SFuncDef argName expr) arg) =
  rewriteBinds argName arg expr
evalExpr (SAppl (unfix -> SBuiltin builtin args) arg) =
  Fix . SBuiltin builtin $ (args ++ [arg])
evalExpr (SAppl (unfix -> SFuncArg s) arg) = Fix $ SAppl (Fix $ SFuncArg s) arg

nEvalExpr (SList rs) = Fix $ NList rs
nEvalExpr (SStr s) = Fix $ NStr s
nEvalExpr (SNumber n) = Fix $ NNumber n
nEvalExpr (SBoolean b) = Fix $ NBoolean b
nEvalExpr (SFuncArg s) = error $ "missed symbol substitution: " ++ s
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
