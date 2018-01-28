module Eval where

import RIO
import AST
import qualified Data.Map as M

type Bindings = Map String AST

eval :: AST -> AST
eval ast = substitute mempty ast

substitute :: Bindings -> AST -> AST
substitute bindings (Symbol name) =
  case M.lookup name bindings of
    Just expr -> substitute bindings expr
    Nothing -> Builtin name

substitute bindings (Appl h []) = substitute bindings h
substitute bindings (Appl f args) =
  case substitute bindings f of
    Builtin name -> builtin name (substitute bindings <$> args)
    Func funcArgs expr -> do
      let argSymbols = assertBinders <$> funcArgs
          newBindings = bindings <> M.fromList (zip argSymbols args)
       in substitute newBindings expr
    _ -> error $ "expected function not expression: " ++ show (substitute bindings f)
substitute _ a = a

builtin :: String -> [AST] -> AST
builtin "+" [Number a, Number b] = Number (a + b)
builtin "-" [Number a, Number b] = Number (a - b)
builtin "*" [Number a, Number b] = Number (a * b)
builtin "=" [Binder name, expr] = Bindings (M.singleton name expr)
builtin name args = error $ "no symbol in scope for " ++ name ++ ": " ++ show args

assertBinders :: AST -> String
assertBinders (Binder name) = name
assertBinders b = error $ "expected binding symbol; found: " ++ show b
