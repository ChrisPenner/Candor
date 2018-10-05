{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}

module Eval where

import qualified AST as A
import Control.Comonad.Cofree
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified ParseAST as P
import RIO
import qualified SimpleAST as S

eval :: P.ParseAST -> Cofree A.ASTF (A.Bindings _)
eval = cata evalExpr

evalExpr ::
     P.ParseASTF (Cofree A.ASTF (A.Bindings S.SimpleAST))
  -> Cofree A.ASTF (A.Bindings S.SimpleAST)
evalExpr (P.Appl (m :< A.FuncDef run) arg) = run arg

subSymbols :: Cofree A.ASTF (A.Bindings S.SimpleAST) -> S.SimpleAST
subSymbols (_ :< A.List _) = undefined
subSymbols (b :< A.Symbol n) =
  case M.lookup n b of
    Nothing -> error $ "missing: " ++ n
    Just v -> v
subSymbols (b :< A.BindingSymbol n) =
  case M.lookup n b of
    Nothing -> error $ "missing: " ++ n
    Just v -> v
