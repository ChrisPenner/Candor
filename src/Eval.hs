{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}

module Eval where

import AST

-- import Control.Comonad.Cofree
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Compose
import Data.Functor.Foldable
import qualified Data.Map as M
import RIO

eval :: AST -> Reader (Bindings SimpleAST) Prim
eval = fmap (cata sEvalExpr) . cata evalExpr

type WithBindings f = Compose f ((,) (Bindings AST))

tryFind :: String -> Bindings SimpleAST -> SimpleAST
tryFind key m =
  case M.lookup key m of
    Just v -> v
    Nothing -> error $ "couldn't find symbol: " ++ key

-- Appl requires we evaluate the FIRST argument first for a binding,
-- and the second argument first for a function application, so it's poorly
-- formed
evalExpr ::
     ASTF (Reader (Bindings SimpleAST) SimpleAST)
  -> Reader (Bindings SimpleAST) SimpleAST
evalExpr (Appl f arg) = do
  f' <- f
  case unfix f' of
    (SBinding name expr) -> do
      local (<> [(name, expr)]) arg
    (SFuncDef argName expr) -> do
      local (<> [(argName, expr)]) arg
    (SBuiltin func) -> do
      func <$> arg
    _ -> error "got unknown thing in Appl"
evalExpr (List rs) = Fix . SList <$> sequenceA rs
evalExpr (Symbol name) = asks (tryFind name)
evalExpr (BindingSymbol name) = asks (tryFind name)
evalExpr (Str s) = return . Fix $ SStr s
evalExpr (Number n) = return . Fix $ SNumber n
evalExpr (Boolean b) = return . Fix $ SBoolean b
evalExpr (FuncDef arg expr) = do
  val <- asks (tryFind "arg")
  local (<> [(arg, val)]) expr
evalExpr (Binding name expr) = Fix . SBinding name <$> expr

sEvalExpr :: SimpleASTF Prim -> Prim
sEvalExpr (SAppl _ arg) = arg
sEvalExpr (SList rs) = PList rs
sEvalExpr (SStr s) = PStr s
sEvalExpr (SNumber n) = PNumber n
sEvalExpr (SBoolean b) = PBoolean b
-- These last two will be thrown away anyways
sEvalExpr (SFuncDef _ expr) = expr
sEvalExpr (SBinding _ bind) = bind
