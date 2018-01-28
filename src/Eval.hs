module Eval where

import RIO
import AST
import Env
import Control.Monad.State
import Control.Lens
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M

type EvalM a = State Env a

type Bindings = M.Map String AST

globals :: Bindings
globals =
  M.fromList [("+", Builtin add), ("-", Builtin sub)]
    where
      add ([Number m, (Number n)]) = Number (m + n)
      add _ = error "expected numbers in (+)"
      sub ([Number m, Number n]) = Number (m - n)
      sub _ = error "expected numbers in (-)"

eval :: AST -> AST
eval ast = substitute globals ast

substitute :: Bindings -> AST -> AST
substitute bindings (Symbol name) =
  case M.lookup name bindings of
    Just expr -> substitute bindings expr
    Nothing -> error $ name ++ " is not defined"

substitute bindings (Appl (h :| [])) = substitute bindings h
substitute bindings (Appl (f :| args)) =
  case substitute bindings f of
    Builtin f' -> f' $ substitute bindings <$> args
    Func funcArgs expr -> do
      let argSymbols = assertString <$> funcArgs
          newBindings = bindings <> M.fromList (zip argSymbols args)
       in substitute newBindings expr
    _ -> error $ "expected function not expression: " ++ show (substitute bindings f)
substitute bindings a = a

assertString :: AST -> String
assertString (Symbol name) = name
assertString b = error $ "expected string in binding position, found: " ++ show b
